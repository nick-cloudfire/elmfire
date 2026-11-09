"""Check report diagnostics and standalone cache configuration without TeX."""
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / 'tools'))
import check_report_environment as checks


class ReportEnvironmentTests(unittest.TestCase):
    def test_package_scan_ignores_comments_and_archived_cases(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            report = root / 'main_report'
            report.mkdir()
            (report / 'report.tex').write_text(
                '% \\usepackage{unused}\n\\usepackage[option]{fontspec,siunitx}\n')
            legacy = root / 'cases/Verification/__legacy__/old'
            (legacy / 'report').mkdir(parents=True)
            (legacy / 'case.yaml').touch()
            (legacy / 'report/report.tex').write_text('\\usepackage{obsolete}')
            self.assertEqual(checks.required_packages(root), ['fontspec', 'siunitx'])

    def test_font_probe_rejects_substitutions_and_missing_faces(self):
        for faces, expected in (
            (['TeXGyreTermes-Regular', 'TeXGyreTermes-Bold', 'TeXGyreTermes-Italic', 'TeXGyreTermes-BoldItalic'], True),
            (['Liberation Serif'] * 4, False),
            (['TeXGyreTermes-Regular'] * 4, False),
            (['TeXGyreTermes-Regular'], False),
        ):
            with self.subTest(faces=faces), tempfile.TemporaryDirectory() as tmp:
                output = Path(tmp)
                labels = ['regular', 'bold', 'italic', 'bolditalic']
                (output / 'font_probe.log').write_text('\n'.join(
                    f'REPORTFONT:{label}:{name}' for label, name in zip(labels, faces)))
                with patch.object(checks.subprocess, 'run', return_value=SimpleNamespace(
                        returncode=0, stdout='', stderr='')):
                    success, _ = checks.font_probe(output)
                self.assertEqual(success, expected)

    @unittest.skipUnless(shutil.which('perl'), 'latexmk configuration uses Perl')
    def test_cache_configuration_overrides_unusable_system_cache(self):
        with tempfile.TemporaryDirectory() as tmp:
            command = ['perl', '-e', 'do $ARGV[0] or die $@; print "$ENV{TEXMFCACHE} $pdf_mode";',
                       str(ROOT / 'common/latexmkrc')]
            import os
            env = os.environ.copy()
            env.pop('ELMFIRE_TEX_CACHE', None)
            env['TEXMFCACHE'] = '/nonexistent/unwritable/cache'
            result = subprocess.run(command, cwd=tmp, env=env, capture_output=True, text=True, check=True)
            self.assertEqual(result.stdout, 'tex-cache 4')
            self.assertTrue((Path(tmp) / 'tex-cache/texmf-var').is_dir())
            env['ELMFIRE_TEX_CACHE'] = '/absolute/cache'
            result = subprocess.run(command, cwd=tmp, env=env, capture_output=True, text=True)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn('non-hidden relative', result.stderr)

    def test_all_report_directories_have_the_portable_configuration(self):
        expected = (ROOT / 'common/latexmkrc').read_bytes()
        for report in checks.report_directories(ROOT):
            if (report / 'case_report.tex').exists() or report.name == 'main_report':
                with self.subTest(report=report):
                    self.assertEqual((report / '.latexmkrc').read_bytes(), expected)


if __name__ == '__main__':
    unittest.main()
