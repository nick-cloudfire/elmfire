import importlib.util
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


MODULE_PATH = Path(__file__).resolve().parents[1] / "run_all.py"
SPEC = importlib.util.spec_from_file_location("run_all", MODULE_PATH)
RUN_ALL = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(RUN_ALL)


class DiscoverCasesTests(unittest.TestCase):
    def make_case(self, root: Path, relative: str) -> None:
        case = root / relative
        case.mkdir(parents=True)
        (case / "run_case.sh").write_text("#!/usr/bin/env bash\n", encoding="utf-8")

    def test_suite_filtering_happens_before_sharding(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            cases = Path(directory) / "cases"
            self.make_case(cases, "Verification/unit_tests/CASE01_ONE")
            self.make_case(cases, "Verification/coupling_tests/CASE02_TWO")
            self.make_case(cases, "Validation/landscape_scale/fire_one")

            all_cases = RUN_ALL.discover_cases(str(cases), "all")
            verification = RUN_ALL.discover_cases(str(cases), "verification")
            validation = RUN_ALL.discover_cases(str(cases), "validation")

            self.assertEqual(len(all_cases), 3)
            self.assertEqual(len(verification), 2)
            self.assertEqual(len(validation), 1)
            start, end = RUN_ALL.shard_slice(len(verification), 2, 1)
            self.assertEqual(verification[start:end], [verification[1]])

    def test_templates_and_legacy_material_are_excluded(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            cases = Path(directory) / "cases"
            self.make_case(cases, "case_template")
            self.make_case(cases, "Verification/coupling_tests/__legacy__/old_case")
            self.make_case(cases, "Validation/structure_scale/current_case")

            discovered = RUN_ALL.discover_cases(str(cases), "all")
            self.assertEqual(len(discovered), 1)
            self.assertIn("current_case", discovered[0])

    def test_unknown_scope_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            with self.assertRaises(ValueError):
                RUN_ALL.discover_cases(str(Path(directory) / "cases"), "other")

    def test_slurm_wrapper_executes_original_runner_without_embedding_it(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            case = root / "case with spaces"
            case.mkdir()
            marker = "RUN_CASE_BODY_MUST_NOT_BE_EMBEDDED"
            (case / "run_case.sh").write_text(
                f"#!/usr/bin/env bash\necho {marker}\n", encoding="utf-8"
            )
            header = root / "slurm_head.txt"
            header.write_text(
                "#SBATCH --job-name=elmfire-generic\n"
                "#SBATCH --ntasks=1\n"
                "module() { :; }\nmodule purge\n",
                encoding="utf-8",
            )

            wrapper = Path(RUN_ALL.make_slurm_wrapper(str(case), str(header)))
            text = wrapper.read_text(encoding="utf-8")

            self.assertNotIn(marker, text)
            self.assertNotIn("elmfire-generic", text)
            self.assertEqual(text.count("#SBATCH --job-name="), 1)
            self.assertIn("#SBATCH --job-name=elmfire-case-with-spaces", text)
            # Replacing the original runner after generation proves the wrapper
            # executes that file and propagates its working directory and exit code.
            (case / "run_case.sh").write_text('printf "%s\\n" "$PWD"; exit 23\n')
            result = subprocess.run(["bash", str(wrapper)], cwd=root,
                                    capture_output=True, text=True)
            self.assertEqual(result.returncode, 23, result.stderr)
            self.assertEqual(Path(result.stdout.strip()).resolve(), case.resolve())
            self.assertTrue(wrapper.stat().st_mode & 0o100)

    def test_suite_specific_slurm_headers(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            cases = root / "cases"
            common = root / "common"
            verification = cases / "Verification/unit_tests/CASE01_ONE/run_case.sh"
            validation = cases / "Validation/landscape_scale/fire/run_case.sh"

            self.assertEqual(
                Path(RUN_ALL.slurm_header_for_case(verification, cases, common)).name,
                "slurm_verification_head.txt",
            )
            self.assertEqual(
                Path(RUN_ALL.slurm_header_for_case(validation, cases, common)).name,
                "slurm_validation_head.txt",
            )

    def test_slurm_job_name_preserves_canonical_case_number(self) -> None:
        self.assertEqual(
            RUN_ALL.slurm_job_name("/suite/cases/CASE08_FBC"),
            "elmfire-CASE08_FBC",
        )
        self.assertEqual(
            RUN_ALL.slurm_job_name("/suite/cases/camp_fire"),
            "elmfire-camp_fire",
        )

    def test_validation_slurm_header_uses_one_rank_per_member(self) -> None:
        header = MODULE_PATH.parents[1] / "common/slurm_validation_head.txt"
        directives = dict(re.findall(r"(?m)^#SBATCH\s+--([\w-]+)=(\S+)", header.read_text()))
        self.assertEqual({key: directives[key] for key in ("nodes", "ntasks", "ntasks-per-node")},
                         {"nodes": "1", "ntasks": "50", "ntasks-per-node": "50"})
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            base, active = root / "base", root / "active"
            (base / "etc/profile.d").mkdir(parents=True)
            (base / "etc/profile.d/conda.sh").touch()
            (active / "bin").mkdir(parents=True)
            python = active / "bin/python"
            python.write_text("#!/usr/bin/env bash\nexit 0\n")
            python.chmod(0o700)
            for allocation in (None, "12"):
                with self.subTest(slurm_tasks=allocation):
                    env = dict(os.environ, TEST_BASE=str(base), TEST_ACTIVE=str(active),
                               TEST_HEADER=str(header), ELMFIRE_VNV_CONDA_ENV="test-env",
                               ELMFIRE_MPI_RANKS="51", PROJ_NETWORK="ON")
                    env.pop("SLURM_NTASKS", None)
                    if allocation is not None:
                        env["SLURM_NTASKS"] = allocation
                    script = ("module() { :; }\n"
                              'conda() { if [[ "$1" == info ]]; then echo "$TEST_BASE"; '
                              'else export CONDA_PREFIX="$TEST_ACTIVE"; fi; }\n'
                              'source "$TEST_HEADER"\nenv\n')
                    result = subprocess.run(["bash", "-c", script], env=env,
                                            capture_output=True, text=True)
                    self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                    observed = dict(line.split("=", 1) for line in result.stdout.splitlines() if "=" in line)
                    self.assertEqual(observed["ELMFIRE_MPI_RANKS"], allocation or "50")
                    self.assertEqual(observed["PROJ_NETWORK"], "OFF")
                    self.assertEqual(observed["PYTHONNOUSERSITE"], "1")
                    self.assertEqual(observed["PYTHON_BIN"], str(python))

    def test_landscape_runners_launch_and_compile_from_another_directory(self) -> None:
        cases = MODULE_PATH.parents[1] / "cases/Validation/landscape_scale"
        for case_id in ("camp_fire", "thomas_fire", "tubbs_fire"):
            for ranks in (None, "1", "4", "0", "51", "invalid"):
                with self.subTest(case=case_id, ranks=ranks), tempfile.TemporaryDirectory() as directory:
                    root = Path(directory).resolve()
                    case, bins = root / "case with spaces", root / "bin"
                    case.mkdir()
                    bins.mkdir()
                    shutil.copy2(cases / case_id / "run_case.sh", case / "run_case.sh")
                    compile_script = case / "compile_case.sh"
                    compile_script.write_text('echo compiled > "$TEST_COMPILE_MARKER"\n')
                    compile_script.chmod(0o600)  # Explicitly require invocation via Bash.
                    trace, marker = root / "trace.jsonl", root / "compiled"
                    for name in ("python-probe", "elmfire", "custom-elmfire", "mpirun"):
                        executable = bins / name
                        executable.write_text(
                            f"#!{sys.executable}\n"
                            "import json, os, sys\nfrom pathlib import Path\n"
                            "name = Path(sys.argv[0]).name\n"
                            "with open(os.environ['TEST_TRACE'], 'a') as stream:\n"
                            " stream.write(json.dumps({'program': name, 'args': sys.argv[1:], "
                            "'cwd': os.getcwd(), 'proj': os.environ.get('PROJ_NETWORK')}) + '\\n')\n"
                            "if name == 'mpirun': os.execvp(sys.argv[3], sys.argv[3:])\n"
                        )
                        executable.chmod(0o700)
                    env = dict(os.environ, PATH=str(bins) + os.pathsep + os.environ["PATH"],
                               PYTHON_BIN=str(bins / "python-probe"), TEST_TRACE=str(trace),
                               TEST_COMPILE_MARKER=str(marker), PROJ_NETWORK="ON")
                    env.pop("ELMFIRE_MPI_RANKS", None)
                    env.pop("ELMFIRE_BIN", None)
                    if ranks is not None:
                        env["ELMFIRE_MPI_RANKS"] = ranks
                        env["ELMFIRE_BIN"] = str(bins / "custom-elmfire")
                    result = subprocess.run(["bash", str(case / "run_case.sh")], cwd=root,
                                            env=env, capture_output=True, text=True)
                    if ranks in ("0", "51", "invalid"):
                        self.assertEqual(result.returncode, 2)
                        self.assertFalse(trace.exists())
                        self.assertFalse(marker.exists())
                        continue
                    error_log = case / "logs/elmfire.stderr"
                    self.assertEqual(result.returncode, 0, result.stdout + result.stderr
                                     + (error_log.read_text() if error_log.exists() else ""))
                    events = [json.loads(line) for line in trace.read_text().splitlines()]
                    programs = [event["program"] for event in events]
                    binary = "elmfire" if ranks is None else "custom-elmfire"
                    self.assertEqual(programs, ["python-probe"] + ([] if ranks == "1" else ["mpirun"])
                                     + [binary, "python-probe"])
                    self.assertEqual(events[0]["args"], [str(case / "scripts/preprocess.py")])
                    self.assertEqual(events[-1]["args"], [str(case / "scripts/postprocess.py")])
                    model = next(event for event in events if event["program"] == binary)
                    self.assertEqual(model["args"], ["elmfire.data.in"])
                    self.assertEqual(Path(model["cwd"]).resolve(), case)
                    self.assertTrue(all(event["proj"] == "OFF" for event in events))
                    if ranks != "1":
                        self.assertEqual(events[1]["args"][:2], ["-np", ranks or "50"])
                    self.assertTrue(marker.is_file())


if __name__ == "__main__":
    unittest.main()
