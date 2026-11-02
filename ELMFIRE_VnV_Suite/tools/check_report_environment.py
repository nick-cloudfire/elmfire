#!/usr/bin/env python3
"""Diagnose report tools, TeX packages, Python, font faces and cache permissions."""
from __future__ import annotations

import argparse
import importlib.util
from pathlib import Path
import re
import shutil
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]


def report_directories(root: Path) -> list[Path]:
    reports = [root / 'main_report', root / 'cases/case_template/report']
    for case in (root / 'cases').rglob('case.yaml'):
        if '__legacy__' not in case.parts and case.parent.name != 'case_template':
            reports.append(case.parent / 'report')
    return reports


def required_packages(root: Path) -> list[str]:
    packages = set()
    pattern = re.compile(r'\\(?:usepackage|RequirePackage)(?:\[[^\]]*\])?\{([^}]+)\}')
    for report in report_directories(root):
        for path in report.glob('*.tex'):
            source = re.sub(r'(?<!\\)%[^\n]*', '', path.read_text(errors='replace'))
            for match in pattern.finditer(source):
                packages.update(p.strip() for p in match.group(1).split(','))
    return sorted(packages)


def font_probe(output: Path) -> tuple[bool, str]:
    output.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(ROOT / 'main_report/report_style.tex', output / 'report_style.tex')
    (output / 'font_probe.tex').write_text(r'''\documentclass[12pt]{article}
\input{report_style.tex}
\newcommand{\CheckFace}[1]{\directlua{texio.write_nl("REPORTFONT:#1:" .. font.getfont(font.current()).fullname)}}
\begin{document}
\CheckFace{regular}TeX Gyre Termes regular.\par
{\bfseries\CheckFace{bold}TeX Gyre Termes bold.\par}
{\itshape\CheckFace{italic}TeX Gyre Termes italic.\par}
{\bfseries\itshape\CheckFace{bolditalic}TeX Gyre Termes bold italic.\par}
\end{document}
''')
    result = subprocess.run(['latexmk', '-r', str(ROOT / 'common/latexmkrc'),
                             '-g', '-lualatex', '-interaction=nonstopmode',
                             '-halt-on-error', 'font_probe.tex'], cwd=output,
                            capture_output=True, text=True, timeout=300)
    log = result.stdout + result.stderr
    (output / 'build.log.txt').write_text(log)
    if result.returncode:
        return False, f'LuaLaTeX/font probe failed. Full log: {output / "build.log.txt"}'
    tex_log = (output / 'font_probe.log').read_text(errors='replace')
    faces = {face: name.strip() for face, name in re.findall(
        r'REPORTFONT:(regular|bold|italic|bolditalic):([^\r\n\[]+)', tex_log)}
    if set(faces) != {'regular', 'bold', 'italic', 'bolditalic'}:
        return False, f'Could not verify all four font faces; inspect {output / "build.log.txt"}'
    if any('texgyretermes' not in re.sub(r'\s+', '', name).lower() for name in faces.values()):
        return False, f'Unexpected font substitution: {faces}'
    if len(set(faces.values())) != 4:
        return False, f'Font faces are not distinct: {faces}'
    return True, 'TeX Gyre Termes regular, bold, italic and bold italic compiled successfully.'


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--tex-only', action='store_true', help='skip aggregate-generator Python dependencies')
    parser.add_argument('--output-dir', type=Path, default=ROOT / 'tmp/report-environment')
    args = parser.parse_args()
    errors = []
    packages_ready = False
    for tool in ('latexmk', 'lualatex', 'kpsewhich'):
        path = shutil.which(tool)
        if path:
            print(f'[OK] {tool}: {path}', flush=True)
        else:
            errors.append(f'Missing executable: {tool}')
    if not args.tex_only and importlib.util.find_spec('yaml') is None:
        errors.append(f'PyYAML is unavailable in {sys.executable}. Activate the suite environment '
                      'and set PYTHON_BIN to its Python, or install PyYAML there.')
    if shutil.which('kpsewhich'):
        missing = []
        for package in required_packages(ROOT):
            result = subprocess.run(['kpsewhich', package + '.sty'], capture_output=True, text=True)
            if result.returncode or not result.stdout.strip():
                missing.append(package)
        if missing:
            errors.append('Missing TeX packages: ' + ', '.join(missing))
        else:
            packages_ready = True
            print('[OK] All TeX packages referenced by current reports are installed.', flush=True)
    if packages_ready and all(shutil.which(t) for t in ('latexmk', 'lualatex', 'kpsewhich')):
        try:
            success, message = font_probe(args.output_dir.resolve())
        except (OSError, subprocess.TimeoutExpired) as exc:
            success, message = False, str(exc)
        if success:
            print('[OK] ' + message)
        else:
            errors.append(message)
            errors.append('Install the TeX Live tex-gyre package (Ubuntu/Debian: fonts-texgyre) '
                          'and check kpsewhich texgyretermes-regular.otf. '
                          'Use a non-hidden relative ELMFIRE_TEX_CACHE such as tex-cache if the log reports a cache failure.')
    if errors:
        for error in errors:
            print('[ERROR] ' + error, file=sys.stderr)
        print('See docs/report-build-troubleshooting.md for installation and repair commands.', file=sys.stderr)
        raise SystemExit(1)
    print('[OK] Report environment ready. Python: ' + sys.executable)


if __name__ == '__main__':
    main()
