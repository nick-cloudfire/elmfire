from __future__ import annotations

import os
import re
import time
import csv
import subprocess
from pathlib import Path
from typing import Dict, List, Tuple, Optional

import psutil
import matplotlib.pyplot as plt


def run_elmfire_memopt_sweep(
    workdir: str | Path = r"home/nick/elmfire_examples/CalibrateMemopt",
    data_filename: str = "memopt.data",
    param_name: str = "WX_BANDS_KEPT_IN_MEM",
    start: int = 10,
    end: int = 500,
    step: int = 10,
    sample_interval_s: float = 0.1,
    warmup_s: float = 0.0,
    results_csv: Optional[str | Path] = "elmfire_memopt_sweep_results.csv",
    keep_backup: bool = True,
) -> Dict[str, List[float]]:
    """
    Runs ELMFIRE in a loop, updating `param_name` in `data_filename` each iteration.

    Per iteration:
      - edits line: WX_BANDS_KEPT_IN_MEM = <number> (treated as plain text)
      - runs: elmfire memopt.data (inside `workdir`)
      - logs elapsed time and peak RSS RAM usage (in MiB) of the elmfire process

    At the end:
      - writes CSV (optional)
      - produces 2 graphs:
          * elapsed time vs bands kept in mem
          * peak RAM (RSS) vs bands kept in mem

    Returns a dict with keys: "bands", "elapsed_s", "peak_rss_mib", "exit_code".
    """

    workdir = Path(workdir)
    data_path = workdir / data_filename
    if not workdir.exists():
        raise FileNotFoundError(f"Work directory does not exist: {workdir}")
    if not data_path.exists():
        raise FileNotFoundError(f"Data file does not exist: {data_path}")

    # Backup original file so you can restore it afterwards
    backup_path = data_path.with_suffix(data_path.suffix + ".bak")
    if keep_backup and not backup_path.exists():
        backup_path.write_bytes(data_path.read_bytes())

    # Regex to find/replace: PARAM = number (allow spaces)
    pat = re.compile(rf"^(\s*{re.escape(param_name)}\s*=\s*)(\d+)(\s*)$")

    def _set_param_value(file_path: Path, new_value: int) -> None:
        lines = file_path.read_text(encoding="utf-8", errors="replace").splitlines(True)
        replaced = False
        out_lines = []

        for line in lines:
            m = pat.match(line.rstrip("\n"))
            if m and not replaced:
                # Preserve original whitespace and newline
                prefix, _, suffix = m.groups()
                newline = "\n" if line.endswith("\n") else ""
                out_lines.append(f"{prefix}{new_value}{suffix}{newline}")
                replaced = True
            else:
                out_lines.append(line)

        if not replaced:
            raise ValueError(
                f"Could not find a line matching '{param_name} = <number>' in {file_path}"
            )

        file_path.write_text("".join(out_lines), encoding="utf-8")

   
    def _run_and_measure(cmd, cwd, sample_interval_s=0.1):
        t0 = time.perf_counter()
    
        # Don't capture stdout/stderr -> avoids MPI pipe / newline / EOF hangs
        proc = subprocess.Popen(
            cmd,
            cwd=str(cwd),
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            text=False,
        )
    
        ps_root = psutil.Process(proc.pid)
        peak_rss = 0
    
        def _tree_rss_bytes(root: psutil.Process) -> int:
            total = 0
            procs = [root] + root.children(recursive=True)
            for p in procs:
                try:
                    total += p.memory_info().rss
                except psutil.NoSuchProcess:
                    pass
            return total
    
        while True:
            # Sample total RSS of mpiexec + all children (MPI ranks)
            try:
                rss = _tree_rss_bytes(ps_root)
                peak_rss = max(peak_rss, rss)
            except psutil.NoSuchProcess:
                # mpiexec might exit quickly at the end; that's fine
                pass
    
            rc = proc.poll()
            if rc is not None:
                break
    
            time.sleep(sample_interval_s)
    
        elapsed = time.perf_counter() - t0
        peak_rss_mib = peak_rss / (1024 * 1024)
        return elapsed, peak_rss_mib, proc.returncode


    bands: List[int] = []
    elapsed_s: List[float] = []
    peak_rss_mib: List[float] = []
    exit_codes: List[int] = []

    try:
        for v in range(start, end + 1, step):
            _set_param_value(data_path, v)

            # Run ELMFIRE exactly as requested:
            #   cd workdir
            #   elmfire memopt.data
            e, m, code = _run_and_measure(["elmfire", data_filename], workdir)

            bands.append(v)
            elapsed_s.append(e)
            peak_rss_mib.append(m)
            exit_codes.append(code)

            print(f"{param_name}={v:3d} | time={e:8.2f}s | peak_rss={m:10.2f} MiB")

    finally:
        # Restore original file content from backup (optional safety)
        if keep_backup and backup_path.exists():
            data_path.write_bytes(backup_path.read_bytes())

    # Write CSV (optional)
    if results_csv is not None:
        results_csv = Path(results_csv)
        with results_csv.open("w", newline="", encoding="utf-8") as f:
            w = csv.writer(f)
            w.writerow(["bands_kept_in_mem", "elapsed_s", "peak_rss_mib", "exit_code"])
            w.writerows(zip(bands, elapsed_s, peak_rss_mib, exit_codes))
        print(f"\nSaved results to: {results_csv.resolve()}")

    # Plots (matplotlib defaults; no explicit colors)
    plt.figure()
    plt.plot(bands, elapsed_s, marker="o")
    plt.xlabel("WX_BANDS_KEPT_IN_MEM")
    plt.ylabel("Elapsed time (s)")
    plt.title("ELMFIRE runtime vs WX_BANDS_KEPT_IN_MEM")
    plt.grid(True)
    plt.tight_layout()

    plt.figure()
    plt.plot(bands, peak_rss_mib, marker="o")
    plt.xlabel("WX_BANDS_KEPT_IN_MEM")
    plt.ylabel("Peak RSS (MiB)")
    plt.title("ELMFIRE peak RAM vs WX_BANDS_KEPT_IN_MEM")
    plt.grid(True)
    plt.tight_layout()

    plt.show()

    return {
        "bands": bands,
        "elapsed_s": elapsed_s,
        "peak_rss_mib": peak_rss_mib,
        "exit_code": exit_codes,
    }

if __name__ == "__main__":
    results = run_elmfire_memopt_sweep(
        workdir=r"/home/nick/elmfire_examples/CalibrateMemopt"
    )