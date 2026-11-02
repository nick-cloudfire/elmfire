# Standalone report configuration. Exact copies live beside each report.
use File::Path qw(make_path);
use File::Spec;
use File::Temp qw(tempfile);

# LuaLaTeX needs a writable cache before it reads any document or fontspec.
# Keep the cache under the report working directory. TeX's openout_any policy
# can reject hidden or absolute cache paths even when the OS allows writes.
my $report_cache = $ENV{'ELMFIRE_TEX_CACHE'}
    || File::Spec->catdir('tex-cache');
die "[ERROR] Use a non-hidden relative ELMFIRE_TEX_CACHE inside the report directory (for example tex-cache).\n"
    if File::Spec->file_name_is_absolute($report_cache)
       || grep { /^\./ } File::Spec->splitdir($report_cache);
eval {
    make_path($report_cache, {mode => 0700});
    my ($probe, $probe_name) = tempfile('write-check-XXXXXX', DIR => $report_cache, UNLINK => 1);
    close($probe);
    make_path(File::Spec->catdir($report_cache, 'texmf-var'));
    make_path(File::Spec->catdir($report_cache, 'fonts'));
};
die "[ERROR] Cannot write TeX cache $report_cache. Set ELMFIRE_TEX_CACHE to a writable directory.\n$@" if $@;
$ENV{'TEXMFCACHE'} = $report_cache;
$ENV{'TEXMFVAR'} = File::Spec->catdir($report_cache, 'texmf-var');
$ENV{'VARTEXFONTS'} = File::Spec->catdir($report_cache, 'fonts');

# TeX Gyre Termes is loaded by filename from the TeX distribution.
# ELMFIRE_REPORT_FONT_DIR is no longer needed.

$pdf_mode = 4;  # LuaLaTeX, including when latexmk is invoked without -lualatex.
$silent = 1;
$cleanup_includes_cusdep_generated = 1;
