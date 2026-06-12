# Homebrew formula for ELMFIRE
#
# Usage (custom tap):
#   brew tap nick-cloudfire/elmfire https://github.com/nick-cloudfire/elmfire
#   brew install elmfire
#
# Or install directly:
#   brew install nick-cloudfire/elmfire/elmfire

class Elmfire < Formula
  desc "Eulerian Level set Model of FIRE spread"
  homepage "https://github.com/nick-cloudfire/elmfire"
  url "https://github.com/nick-cloudfire/elmfire/archive/refs/heads/main.tar.gz"
  version "2026.0519"
  license "BSD-3-Clause"
  head "https://github.com/nick-cloudfire/elmfire.git", branch: "main"

  # Runtime and build dependencies
  depends_on "gcc"       # provides gfortran
  depends_on "open-mpi"  # provides mpifort + mpirun
  depends_on "gdal"      # provides gdal_translate, gdaltransform, etc.

  # open-mpi must be built with the same gcc to avoid ABI mismatch
  fails_with :clang do
    cause "ELMFIRE requires gfortran from Homebrew gcc, not Apple Clang."
  end

  def install
    # Point build scripts at the Homebrew-installed gfortran.
    # gcc is keg-only, so we need the versioned binary.
    gcc_ver = Formula["gcc"].version.major
    ENV["ELMFIRE_FCOMPL_MPI_GNU"]    = "mpifort"
    ENV["ELMFIRE_FCOMPL_SERIAL_GNU"] = "gfortran-#{gcc_ver}"
    ENV["ELMFIRE_INSTALL_DIR"]       = (prefix/"bin").to_s

    (prefix/"bin").mkpath

    # Run the macOS build script from a temporary working directory
    # so object files don't pollute the source tree.
    build_dir = buildpath/"_build"
    build_dir.mkpath
    Dir.chdir(build_dir) do
      system "bash", buildpath/"build/macos/make_gnu.sh"
    end
  end

  def post_install
    # Symlink unversioned binary names into the Homebrew bin prefix
    (HOMEBREW_PREFIX/"bin").install_symlink Dir["#{bin}/elmfire", "#{bin}/elmfire_post"].select { |f| File.exist?(f) }
  end

  test do
    assert_predicate bin/"elmfire", :exist?, "elmfire binary not found"
    # elmfire exits non-zero without input, but should print usage/version
    output = shell_output("#{bin}/elmfire 2>&1", 1)
    assert_match(/elmfire/i, output)
  end

  def caveats
    <<~EOS
      ELMFIRE requires GDAL CLI tools and an MPI runtime at runtime.
      These are installed automatically as Homebrew dependencies.

      Set PATH_TO_GDAL in your ELMFIRE namelist input to:
        #{Formula["gdal"].opt_bin}

      To run in parallel (N processes):
        mpirun -np N elmfire <input.nml>
    EOS
  end
end
