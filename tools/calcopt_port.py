#!/usr/bin/env python3
"""Faithful Python port of calcoptBLPComp (extracted from the WDX blob inside
Calculator.wls / Calculator2.wls of GSUI5051/StarOptimizer, upstream 1ekf/ExidleStars).

Reproduces the Star Optimizer's recommendation table so we can debug the
"recommends not-yet-unlocked variables" issue offline.
"""
import math
import sys

# --- tables extracted from the compiled function ---------------------------
A1 = [0.5, 0.625, 0.78125, 0.8931739295455593, 1.0211323755774153, 1.16742248509526,
      1.3346705004190407, 1.459278106369075, 1.6315225219592102, 1.865258729062898,
      2.085422656904751, 2.384185791015625, 2.725750517411985, 3.1162487047650607,
      3.407188146764981, 3.9832092821269787, 4.453363361722031, 5.206251464550825,
      5.820766091346741]
B1 = [-4.143474048424998, -3.1709615006770515, 1.732020845644619, 15.973438351860704,
      35.10114920824737, 62.78631425389338, 99.28201254141936, 144.78860583366378,
      199.47274109054294, 263.4775066411068, 336.92853801096174, 512.6072746595227,
      727.2885715481912, 849.4650465308255, 1123.859483302632, 1438.7509481751554,
      1794.6202607002178, 2191.902179662476, 2630.993842401342]
A2 = [2, 2.2, 2.42, 2.662, 2.9282, 3.22102, 3.543122, 3.8974342, 4.28717762,
      4.715895382, 5.1874849202, 5.70623341222, 6.276856753442, 6.9045424287862,
      7.59499667166483, 8.354496338831312, 9.189945972714444, 10.108940569985888,
      11.119834626984478]
B2 = [0.6992669815829572, 0.7336524096644791, 2.7382112580862277, 16.973438351860707,
      36.10114920785727, 63.786314253893394, 100.28201254141943, 145.78860583366372,
      200.47274109054277, 264.47750664110686, 337.92853801096135, 513.6072746595223,
      728.2885715481901, 850.4650465308251, 1124.859483302631, 1439.7509481751538,
      1795.620260700215, 2192.9021796624766, 2631.993842401341]
STAR = [1, 0.27, 0.09, 0.1, 0.12, 0.17, 0.22, 0.5, 0.7, 0.9, 2, 3, 4, 4.5, 6, 7, 8, 9, 10]
SYMS = ["y", "z", "s", "u", "v", "w", "α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι",
        "κ", "λ", "ν", "ξ"]

LOG10_LOG10_2 = math.log10(math.log10(2))     # -0.521440...
LOG2_10 = 1 / math.log10(2)                   # 3.321928...


def frontier(llf, a, b):
    """Ramp[Floor[T + 1]] with T = ((llf - log10(log10 2))*log2(10) - b)/a"""
    t = ((llf - LOG10_LOG10_2) * LOG2_10 - b) / a
    return max(0, math.floor(t + 1))


def g(l):
    """game's effective-exponent piecewise function"""
    if l < 10: return 0
    if l < 25: return 1
    if l < 1500: return l / 25 + 1
    if l < 6000: return l / 50 + 31
    if l < 10000: return l / 100 + 91
    if l < 24000: return l / 200 + 141
    return l / 400 + 201


def powfn(l, e):
    return 2.0 ** (e * math.floor(g(l))) * (l ** e)


def mod_gap(l, n):
    """Mathematica Mod[-l, n, 1]: steps to next multiple of n, in [1, n]"""
    r = (-l) % n            # python Mod in [0, n)
    return r if r >= 1 else n


def d_gap(l):
    if l < 10: return 10 - l
    if l < 25: return 25 - l
    if l < 1500: return mod_gap(l, 25)
    if l < 6000: return mod_gap(l, 50)
    if l < 10000: return mod_gap(l, 100)
    if l < 24000: return mod_gap(l, 200)
    return mod_gap(l, 400)


def star_cost(a, cur, dlv):
    """Sum[Floor[a*(cur+i)*Exp[a*(cur+i)/200] + 1], {i,0,dlv-1}]"""
    tot = 0
    for i in range(dlv):
        x = a * (cur + i)
        tot += math.floor(x * math.exp(x / 200) + 1)
    return tot


def solve_lp(options, budget):
    """Multiple-choice knapsack: per block choose <=1 option; max power; cost<=budget.
    options: list of blocks, each block a list of (cost, power, level).
    Returns list of chosen index per block or None."""
    from scipy.optimize import milp, LinearConstraint, Bounds
    import numpy as np

    cols = []
    blocks = []
    for blk in options:
        start = len(cols)
        cols.extend(blk)
        blocks.append((start, len(cols)))
    n = len(cols)
    if n == 0 or budget <= 0:
        return [None] * len(options)
    # normalize objective like the WL code: c = Chop[-power/Max[power]]
    maxp = max(o[1] for o in cols) or 1.0
    c = -np.array([o[1] / maxp for o in cols], dtype=float)
    c[np.abs(c) < 1e-10] = 0.0
    # normalize like the WL code (Chop[c/Max]) - keep raw; milp handles scale
    rows, lb, ub = [], [], []
    for (s, e) in blocks:
        r = np.zeros(n); r[s:e] = 1
        rows.append(r); lb.append(0); ub.append(1)          # sum <= 1
    r = np.array([o[0] for o in cols], dtype=float)
    rows.append(r); lb.append(-np.inf); ub.append(budget)   # cost <= budget
    A = np.vstack(rows)
    integrality = np.ones(n)
    res = milp(c=c, constraints=LinearConstraint(A, lb, ub),
               integrality=integrality, bounds=Bounds(0, 1))
    if not res.success:
        return [None] * len(options)
    x = np.round(res.x).astype(int)
    chosen = []
    for (s, e) in blocks:
        idx = None
        for j in range(s, e):
            if x[j] == 1:
                idx = j - s
                break
        chosen.append(idx)
    return chosen


def calcopt(llf, initstars, ex, unlocked=None, verbose=False):
    """ex: list of 19 exponents (as built by the wrapper) or int supremacy level.
    unlocked: optional set/list of 19 bools - game-unlock gate (the FIX)."""
    if isinstance(ex, int):
        ex = [9] + [min(1.08, max(1.0, 1 + 0.04 * (ex - 2 * n))) for n in range(0, 18)]
    assert len(ex) == 19

    # ex2/exps
    ex2 = [1.0]
    for e in ex[1:]:
        ex2.append(ex2[-1] * e)
    totex = ex2[-1]
    exps = [v / totex for v in ex2]

    vars_ = [frontier(llf, a, b) for a, b in zip(A1, B1)]
    ups = [frontier(llf, a, b) + 1 for a, b in zip(A2, B2)]
    upsc = [1.0]
    for j in range(1, 19):
        upsc.append(upsc[-1] * (ups[j] ** exps[j - 1]))
    initpows = [u * powfn(v, e) for u, v, e in zip(upsc, vars_, exps)]

    if verbose:
        print("exps:", [round(e, 6) for e in exps])
        print("vars:", vars_)
        print("ups:", ups)

    def allowed(i):
        return True if unlocked is None else bool(unlocked[i])

    # ---- pass 1
    fincosts = [0] * 19
    finlevs = [0] * 19
    finpows = [0.0] * 19
    all_options = []
    for i in range(19):
        opts = []
        prevCost, prevLevel, cost = 0, 0, 0
        while cost < initstars:
            dlv = d_gap(vars_[i] + prevLevel)
            cost = prevCost + star_cost(STAR[i], prevLevel, dlv)
            level = prevLevel + dlv
            power = upsc[i] * powfn(vars_[i] + level, exps[i]) - initpows[i]
            if cost <= initstars:
                opts.append((cost, power, level))
                prevLevel, prevCost = level, cost
        if allowed(i):
            all_options.append(opts)
        else:
            all_options.append([])
    if any(all_options):
        chosen = solve_lp(all_options, initstars)
        for i, ci in enumerate(chosen):
            if ci is not None:
                cost, power, level = all_options[i][ci]
                fincosts[i], finlevs[i], finpows[i] = cost, level, power

    # weak/strong filter
    ratios = [(initpows[i] + finpows[i]) / (vars_[i] + finlevs[i])
              for i in range(19) if (vars_[i] + finlevs[i]) > 0]
    weak_threshold = 2 * max(ratios) if ratios else 0.0
    strong = [1 if (initpows[i] + finpows[i]) > weak_threshold else 0 for i in range(19)]
    for i in range(19):
        if strong[i] == 0:
            fincosts[i], finlevs[i], finpows[i] = 0, 0, 0.0

    # ---- pass 2
    stars = initstars - sum(fincosts)
    all_options2 = []
    for i in range(19):
        opts = []
        prevCost, prevLevel, cost = 0, finlevs[i], 0
        guard = 0
        while cost < stars and guard < 100000:
            guard += 1
            if strong[i] == 0:
                dlv = d_gap(vars_[i] + prevLevel)
                cost = prevCost + star_cost(STAR[i], prevLevel, dlv)
                level = prevLevel + dlv
            else:
                cost = prevCost + math.floor(STAR[i] * prevLevel * math.exp(STAR[i] * prevLevel / 200) + 1) if prevLevel > 0 else 1
                level = prevLevel + 1
            power = upsc[i] * powfn(vars_[i] + level, exps[i]) - (initpows[i] + finpows[i])
            if cost <= stars:
                opts.append((cost, power, level - finlevs[i]))
                prevLevel, prevCost = level, cost
            if strong[i] == 1 and prevLevel > 40000:
                break
        if allowed(i):
            all_options2.append(opts)
        else:
            all_options2.append([])
    if any(all_options2) and stars > 0:
        chosen = solve_lp(all_options2, stars)
        for i, ci in enumerate(chosen):
            if ci is not None:
                cost, power, dlevel = all_options2[i][ci]
                fincosts[i] += cost
                finlevs[i] += dlevel
                finpows[i] += power

    return {"fincosts": fincosts, "vars": vars_, "finlevs": finlevs,
            "initpows": initpows, "finpows": finpows, "exps": exps,
            "ups": ups, "upsc": upsc}


def auto_ex_level(llf):
    """the 'Automatic' third-argument deduction in the wrapper"""
    lpsi = (llf / 25 - 1) / math.log2(10)
    return min(24, max(0, math.floor((lpsi - 90) / 20)))


def report(llf, initstars, ex, unlocked=None):
    r = calcopt(llf, initstars, ex, unlocked)
    totpow = sum(r["initpows"]) + sum(r["finpows"])
    print(f"== llf={llf} stars={initstars} ex={ex if isinstance(ex,int) else 'list'}")
    print(f"   stars used: {sum(r['fincosts'])}  left: {initstars - sum(r['fincosts'])}")
    hdr = f"{'var':>4} {'Δlvl':>6} {'stars':>10} {'lvlBefore':>10} {'lvlAfter':>9} {'pow%':>9}"
    print(hdr)
    for i in range(19):
        p = (r["initpows"][i] + r["finpows"][i]) / totpow * 100
        mark = " <-- recommended" if r["finlevs"][i] > 0 else ""
        print(f"{SYMS[i]:>4} {r['finlevs'][i]:>6} {r['fincosts'][i]:>10} "
              f"{r['vars'][i]:>10} {r['vars'][i]+r['finlevs'][i]:>9} {p:>8.4f}%{mark}")
    return r


if __name__ == "__main__":
    llf = float(sys.argv[1]) if len(sys.argv) > 1 else 500
    stars = int(sys.argv[2]) if len(sys.argv) > 2 else 3000
    exl = sys.argv[3] if len(sys.argv) > 3 else "auto"
    ex = auto_ex_level(llf) if exl == "auto" else int(exl)
    print("auto supremacy level:", ex)
    report(llf, stars, ex)
