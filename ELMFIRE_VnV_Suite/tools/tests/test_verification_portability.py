"""Regression checks for running verification cases outside an ELMFIRE source tree."""

from __future__ import annotations

import re
import ast
import json
import os
import math
import importlib.util
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

import numpy as np
import rasterio
from rasterio.transform import from_origin


ROOT = Path(__file__).resolve().parents[2]
VERIFICATION = ROOT / "cases" / "Verification"
SPOTTING = VERIFICATION / "coupling_tests"
sys.path.insert(0, str(ROOT / "tools"))
from namelist_lib import parse_namelist, parse_scalar


def copy_case_sources(source: Path, destination: Path) -> None:
    """Copy only source material; generated variants cannot help the test pass."""
    destination.mkdir(parents=True, exist_ok=True)
    shutil.copytree(source / "scripts", destination / "scripts",
                    ignore=shutil.ignore_patterns("__pycache__"))
    for name in ("elmfire.data.in", "case.yaml", "case.json"):
        if (source / name).is_file():
            shutil.copy2(source / name, destination / name)
    if (source / "data/misc").is_dir():
        shutil.copytree(source / "data/misc", destination / "data/misc")


def imported_modules(script: Path) -> set[str]:
    """Inspect actual imports without matching comments, strings, or spacing."""
    result = set()
    for node in ast.walk(ast.parse(script.read_text())):
        if isinstance(node, ast.Import):
            result.update(alias.name.split(".")[0] for alias in node.names)
        elif isinstance(node, ast.ImportFrom) and node.module:
            result.add(node.module.split(".")[0])
    return result


class VerificationPortabilityTests(unittest.TestCase):
    def spotting_cases(self) -> list[Path]:
        return sorted(SPOTTING.glob("CASE[01][0-9]_*"))[:14]

    def active_cases(self) -> list[Path]:
        return sorted(
            path.parent
            for category in ("unit_tests", "coupling_tests")
            for path in (VERIFICATION / category).glob("CASE*/run_case.sh")
        )

    def test_all_fourteen_spotting_cases_are_present(self) -> None:
        cases = self.spotting_cases()
        self.assertEqual(len(cases), 14)
        self.assertEqual(
            [case.name[:6] for case in cases],
            [f"CASE{number:02d}" for number in range(1, 15)],
        )

    def test_spotting_templates_use_only_reader_supported_domain_metadata(self) -> None:
        for case in self.spotting_cases():
            namelist = case / "elmfire.data.in"
            if namelist.is_file():
                self.assertNotIn("&COMPUTATIONAL_DOMAIN",
                                 namelist.read_text(encoding="utf-8"), case.name)

    def test_runnable_spotting_cases_own_required_model_tables(self) -> None:
        for case in self.spotting_cases():
            if case.name.startswith("CASE13_"):
                continue  # Capability-only design; it does not launch ELMFIRE.
            names = []
            template = (case / "elmfire.data.in").read_text(encoding="utf-8")
            if re.search(r"(?m)^USE_BLDG_SPREAD_MODEL\s*=\s*\.TRUE\.", template):
                names.append("building_fuel_models.csv")
            if re.search(r"(?m)^FUEL_MODEL_FILE\s*=", template):
                names.append("fuel_models.csv")
            for name in names:
                self.assertTrue((case / "data" / "misc" / name).is_file(),
                                f"{case.name} is missing data/misc/{name}")

    def test_spotting_cases_prepare_without_a_source_checkout(self) -> None:
        for source in self.spotting_cases():
            with self.subTest(case=source.name), tempfile.TemporaryDirectory() as directory:
                case = Path(directory) / "isolated case"
                copy_case_sources(source, case)
                non_wue = source.name[:6] in {f"CASE{i:02d}" for i in range(1, 10)}
                if non_wue and (case / "data/misc").exists():
                    shutil.rmtree(case / "data/misc")
                result = subprocess.run(
                    [sys.executable, str(case / "scripts/preprocess.py")],
                    cwd=directory, capture_output=True, text=True,
                )
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertTrue(list(case.rglob("*.tif")), "No prepared raster evidence")
                if non_wue:
                    self.assertFalse((case / "data/misc/fuel_models.csv").exists())
                    self.assertFalse((case / "data/misc/building_fuel_models.csv").exists())

    def test_runners_reach_case_local_preparation_from_another_directory(self) -> None:
        # Stop at the Python boundary: no preprocessor, simulation, or report runs.
        for source in self.active_cases():
            with self.subTest(case=source.name), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                case = root / "case with spaces"
                copy_case_sources(source, case)
                shutil.copy2(source / "run_case.sh", case / "run_case.sh")
                probe = root / "python-probe"
                probe.write_text(
                    f"#!{sys.executable}\n"
                    "import json, os, sys\n"
                    "from pathlib import Path\n"
                    "Path(os.environ['PROBE_LOG']).write_text(json.dumps({"
                    "'script': str(Path(sys.argv[1]).resolve()), "
                    "'no_user_site': os.environ.get('PYTHONNOUSERSITE')}))\n"
                    "sys.exit(37)\n"
                )
                probe.chmod(0o700)
                log = root / "probe.json"
                env = dict(os.environ, PYTHON_BIN=str(probe), PROBE_LOG=str(log),
                           PYTHONNOUSERSITE="0", ELMFIRE_BIN="must-not-run")
                result = subprocess.run(["bash", str(case / "run_case.sh")],
                                        cwd=root, env=env, capture_output=True, text=True)
                self.assertEqual(result.returncode, 37, result.stdout + result.stderr)
                observed = json.loads(log.read_text())
                script = Path(observed["script"])
                self.assertTrue(script.is_relative_to((case / "scripts").resolve()), observed)
                self.assertTrue(script.is_file(), observed)
                self.assertEqual(observed["no_user_site"], "1")

    def test_case_python_does_not_require_gdal_bindings(self) -> None:
        """Keep case scripts independent of the ABI-sensitive osgeo package."""
        for case in self.active_cases():
            for script in (case / "scripts").glob("*.py"):
                self.assertNotIn("osgeo", imported_modules(script), str(script))

    def test_case_namelists_do_not_hard_code_system_gdal(self) -> None:
        """Let ELMFIRE resolve the GDAL utilities from the active environment."""
        for case in self.active_cases():
            for namelist in (case / "elmfire.data.in", case / "elmfire.data"):
                if not namelist.is_file():
                    continue
                source = namelist.read_text(encoding="utf-8")
                self.assertNotRegex(
                    source,
                    r"(?mi)^\s*PATH_TO_GDAL\s*=\s*['\"]?/usr/bin/?['\"]?",
                    str(namelist),
                )

    def test_clean_preparation_produces_bounded_phi_and_valid_timesteps(self) -> None:
        cases = [f"unit_tests/CASE{number}_{suffix}" for number, suffix in
                 ((34, "FMS"), (35, "WSS"), (36, "SLS"), (37, "DMS"),
                  (38, "LMS"), (39, "DHC"), (41, "CFP"))]
        cases += ["coupling_tests/CASE42_WAF", "coupling_tests/CASE40_WSD",
                  "coupling_tests/CASE31_PFT"]
        for relative in cases:
            with self.subTest(case=relative), tempfile.TemporaryDirectory() as directory:
                case = Path(directory) / "isolated case"
                copy_case_sources(VERIFICATION / relative, case)
                result = subprocess.run([sys.executable, str(case / "scripts/preprocess.py")],
                                        cwd=directory, capture_output=True, text=True)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                decks = sorted((case / "variants").glob("*/elmfire.data"))
                self.assertTrue(decks, relative)
                for deck in decks:
                    with self.subTest(variant=deck.parent.name):
                        with rasterio.open(deck.parent / "inputs/phi.tif") as raster:
                            phi = raster.read(1)
                            self.assertTrue(raster.crs)
                            self.assertTrue(np.all(np.isfinite(phi)))
                            self.assertGreaterEqual(float(phi.min()), -1.0)
                            self.assertLessEqual(float(phi.max()), 1.0)
                            self.assertTrue(np.any((phi > -1.0) & (phi < 0.0)))
                            self.assertTrue(np.any((phi > 0.0) & (phi < 1.0)))
                            self.assertFalse(np.any(np.isclose(phi, 0.0, atol=1e-7)))
                            border = np.concatenate((phi[0], phi[-1], phi[:, 0], phi[:, -1]))
                            self.assertTrue(np.all(border > 0.0))
                        values = {a.key: parse_scalar(a.value) for a in parse_namelist(deck)}
                        dt, dtmax, stop = (values[k] for k in
                                          ("SIMULATION_DT", "SIMULATION_DTMAX", "SIMULATION_TSTOP"))
                        self.assertTrue(all(math.isfinite(v) and v > 0 for v in (dt, dtmax, stop)))
                        self.assertLessEqual(dt, dtmax)
                        self.assertAlmostEqual(stop / dt, round(stop / dt), places=6)
                if not relative.endswith("CASE31_PFT"):
                    manifest = json.loads((case / "variants/expected.json").read_text())
                    for variant in manifest["variants"]:
                        deck = case / "variants" / variant["id"] / "elmfire.data"
                        values = {a.key: parse_scalar(a.value) for a in parse_namelist(deck)}
                        speed = variant.get("expected_ros_m_min", variant.get("expected_head_ros_m_min"))
                        with rasterio.open(deck.parent / "inputs/phi.tif") as raster:
                            cell_size = abs(raster.transform.a)
                        advance = speed / 60.0 * values["SIMULATION_DT"] / cell_size
                        self.assertLessEqual(advance, values["TARGET_CFL"] + 1e-6)

    def test_case_variants_are_not_nested_under_data(self) -> None:
        for case in self.active_cases():
            self.assertFalse(
                (case / "data/variants").exists(),
                f"{case.name} stores generated variants below data/",
            )

    def test_case19_reads_both_dump_names_and_physical_times(self) -> None:
        path = SPOTTING / "CASE19_WTH/scripts/raster_functions.py"
        spec = importlib.util.spec_from_file_location("case19_raster_test", path)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        for marker in ("", "d"):
            with self.subTest(marker=marker), tempfile.TemporaryDirectory() as directory:
                output = Path(directory)
                for index, value in ((2, 20.0), (1, 10.0)):
                    with rasterio.open(output / f"heat_0000001_{marker}{index:07d}.tif", "w",
                                       driver="GTiff", width=2, height=2, count=1,
                                       dtype="float32", crs="EPSG:32610",
                                       transform=from_origin(0, 20, 10, 10)) as dst:
                        dst.write(np.full((2, 2), value, dtype="float32"), 1)
                (output / "dump_times_0000001.csv").write_text(
                    "dump_index,time_seconds,is_final_dump\n1,30,F\n2,60,T\n")
                stack, times, *_ = module.load_stack(str(output / "heat_*.tif"))
                np.testing.assert_array_equal(times, [30, 60])
                np.testing.assert_array_equal(stack[:, 0, 0], [10, 20])
                # A second mapping is ambiguous evidence and must be rejected.
                shutil.copy2(output / "dump_times_0000001.csv", output / "dump_times_0000002.csv")
                with self.assertRaises(ValueError):
                    module.load_stack(str(output / "heat_*.tif"))

    def test_guide_variant_namelists_have_sources_outside_variants(self) -> None:
        for number in range(20, 30):
            matches = list(SPOTTING.glob(f"CASE{number:02d}_*"))
            self.assertEqual(len(matches), 1, f"CASE{number:02d}")
            case = matches[0]
            for raw in (case / "scripts/variants.tsv").read_text(
                encoding="utf-8"
            ).splitlines():
                if not raw.strip() or raw.lstrip().startswith("#"):
                    continue
                label, config_rel, _ = raw.split("\t")
                if not config_rel.startswith("variants/"):
                    continue
                special = case / "scripts/namelists" / f"{label}.in"
                self.assertTrue(
                    special.is_file() or (case / "elmfire.data.in").is_file(),
                    f"{case.name}/{label} has no source namelist",
                )

    def test_adapter_modules_are_case_local(self) -> None:
        for case in self.active_cases():
            scripts = case / "scripts"
            modules = set().union(*(imported_modules(p) for p in scripts.glob("*.py")))
            for module in ("case_adapter", "guide_verification"):
                if module in modules:
                    self.assertTrue((scripts / f"{module}.py").is_file(),
                                    f"{case.name} imports non-local {module}")


if __name__ == "__main__":
    unittest.main()
