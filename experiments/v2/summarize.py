"""Summarize run1.sh output: one row per (n, m, J, B, variant, samples, dtau).

Columns: runs, runs that stopped with an error (FAIL), runs that ended in NaN, the median relative error of the
final Monte Carlo energy in percent with its range over seeds, the median error of the exact energy of the trained
state (n <= 16), the number of runs whose Monte Carlo energy is below E0, and the median reported standard error
divided by the spread of final energies over seeds.
"""
import collections, re, statistics, sys

rows = collections.defaultdict(list)
for line in open(sys.argv[1]):
    if 'RESULT' not in line and 'FAIL' not in line:
        continue
    kv = dict(re.findall(r'(\w+)=\s*(\S+)', line))
    key = (int(kv['n']), int(kv['m']), float(kv['J']), float(kv['B']), kv['variant'], int(kv['samples']), float(kv['dtau']))
    rows[key].append((kv, 'FAIL' in line, line.strip()))

order = ['v2', 'nogauge', 'no_SAMPLER', 'no_THETA', 'no_BCONJ', 'no_ELOC', 'no_STABLE', 'none', 'v2_dp', 'fsr']
med = lambda xs: statistics.median(xs) if xs else float('nan')
print(f"{'n':>4} {'m':>3} {'J':>5} {'B':>5} {'variant':>10} {'smp':>4} {'dtau':>5} {'runs':>4} {'FAIL':>4} {'NaN':>4} "
      f"{'mc err %':>9} {'(min..max)':>19} {'state err %':>11} {'<E0':>4} {'se/spread':>9} {'time s':>7}")
for key in sorted(rows, key=lambda k: (k[0], k[1], k[2], k[3], order.index(k[4]), k[5], k[6])):
    n, m, J, B, v, ns, dt = key
    rs = rows[key]
    fails = [r for r, f, _ in rs if f]
    ok = [r for r, f, _ in rs if not f]
    good = [r for r in ok if r['E_mc'].lower() != 'nan']
    mc = [100*float(r['relerr_mc']) for r in good]
    st = [100*float(r['relerr_state']) for r in good if float(r['relerr_state']) != -1.0]
    below = sum(float(r['E_mc']) < float(r['E0']) for r in good)
    se = med([float(r['se']) for r in good])
    spread = statistics.stdev([float(r['E_mc']) for r in good]) if len(good) > 1 else float('nan')
    ratio = se/spread if spread and spread > 0 else float('nan')
    t = med([float(r['time']) for r, f, _ in rs])
    rng = f"({min(mc):.4f}..{max(mc):.4f})" if mc else ''
    print(f"{n:>4} {m:>3} {J:>5} {B:>5} {v:>10} {ns:>4} {dt:>5} {len(rs):>4} {len(fails):>4} {len(ok)-len(good):>4} "
          f"{med(mc):>9.4f} {rng:>19} {med(st):>11.4f} {below:>4} {ratio:>9.3f} {t:>7.0f}")
fails = [l for k in rows for r, f, l in rows[k] if f]
if fails:
    print('\nFailures:'); print('\n'.join(sorted(set(re.sub(r'time=\S+ ', '', l) for l in fails))))
