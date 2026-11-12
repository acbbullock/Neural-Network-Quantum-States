"""Summarize RESULT lines from run1.sh: one row per (variant, n, B, early exit)."""
import collections, math, re, statistics, sys

rows = collections.defaultdict(list)
for line in open(sys.argv[1]):
    if 'RESULT' not in line:
        continue
    kv = dict(re.findall(r'(\w+)=\s*(\S+)', line))
    key = (int(kv['n']), int(kv['m']), float(kv['B']), kv['variant'], int(kv['early']), int(kv.get('samples', 16)), float(kv.get('dtau', 0.01)))
    rows[key].append(kv)

def med(xs):
    return statistics.median(xs) if xs else float('nan')

order = {'base': 0, 'theta': 1, 'all': 2, 'samp': 3, 'all5': 4, 'all6': 5, 'sr': 6, 'srnk': 7, 'srnk_dp': 8, 'fsr': 9, 'nk_only': 10, 'no_SAMPLER': 11, 'no_THETA': 12, 'no_BCONJ': 13, 'no_ELOC': 14, 'no_STABLE': 15, 'clean': 16}
print(f"{'n':>5} {'m':>3} {'B':>4} {'variant':>10} {'exit':>4} {'smp':>4} {'dtau':>5} {'runs':>4} {'NaN':>4} {'epochs':>6} "
      f"{'mc err %':>9} {'(min..max)':>17} {'state err %':>11} {'below E0':>8} {'se/spread':>9} {'drift':>8}")
for key in sorted(rows, key=lambda k: (k[0], k[2], k[1], order[k[3]], -k[4], k[5], k[6])):
    n, mm, B, v, early, ns, dt = key
    rs = rows[key]
    good = [r for r in rs if r['E_mc'].lower() != 'nan']
    mc = [100 * float(r['relerr_mc']) for r in good]
    st = [100 * float(r['relerr_state']) for r in good if float(r['relerr_state']) != -1.0]
    below = sum(1 for r in good if float(r['E_mc']) < float(r['E0']))
    ep = med([int(r['epochs']) for r in rs])
    # Reported standard error against the spread of final energies across seeds
    se = med([float(r['se']) for r in good])
    spread = statistics.stdev([float(r['E_mc']) for r in good]) if len(good) > 1 else float('nan')
    ratio = se / spread if spread and spread > 0 else float('nan')
    drift = med([float(r['drift']) for r in rs])
    rng = f"({min(mc):.2f}..{max(mc):.2f})" if mc else ''
    print(f"{n:>5} {mm:>3} {B:>4} {v:>10} {('on' if early else 'off'):>4} {ns:>4} {dt:>5} {len(rs):>4} {len(rs)-len(good):>4} {ep:>6.0f} "
          f"{med(mc):>9.3f} {rng:>17} {med(st):>11.3f} {below:>8} {ratio:>9.3f} {drift:>8.2g}")
