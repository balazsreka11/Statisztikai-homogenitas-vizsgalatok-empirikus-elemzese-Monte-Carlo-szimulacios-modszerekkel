#%% Csomagok betöltése
import numpy as np
import pandas as pd
from scipy.stats import (t, logistic, laplace, gumbel_r, expon, gamma, lognorm, weibull_min,
                         uniform, beta, norm, chi2_contingency, ks_2samp, kruskal)
from itertools import product
from scipy.integrate import quad


#%% Eloszlásminták előállítása

# Keverékeloszlások létrehozása adott paraméterekkel
def mixture_normal(n, mu, sigma, eps):
    n1 = int(n * (1 - eps))
    n2 = n - n1
    sample1 = norm(loc=0, scale=1).rvs(size=n1)
    sample2 = norm(loc=mu, scale=sigma).rvs(size=n2)
    return np.concatenate([sample1, sample2])

# Eloszlások definiálása
DIST = {
    "Student-féle t-eloszlás (df=1)":            lambda n: t(df=1).rvs(size=n),
    "Student-féle t-eloszlás (df=3)":            lambda n: t(df=3).rvs(size=n),

    "Standard logisztikus":  lambda n: logistic(loc=0, scale=1).rvs(size=n),
    "Standard Laplace":   lambda n: laplace(loc=0, scale=1).rvs(size=n),

    "Gumbel(0, 1)":  lambda n: gumbel_r(loc=0, scale=1).rvs(size=n),
    "Gumbel(0, 2)":  lambda n: gumbel_r(loc=0, scale=2).rvs(size=n),
    "Gumbel(0, 0.5)":lambda n: gumbel_r(loc=0, scale=0.5).rvs(size=n),

    "Exponenciális (1)":     lambda n: expon(scale=1).rvs(size=n),

    "Gamma (2, 1)":   lambda n: gamma(a=2, scale=1).rvs(size=n),
    "Gamma (0.5, 1)": lambda n: gamma(a=0.5, scale=1).rvs(size=n),

    "Lognormális (0, 1)": lambda n: lognorm(s=1, scale=np.exp(0)).rvs(size=n),
    "Lognormális (0, 2)": lambda n: lognorm(s=2, scale=np.exp(0)).rvs(size=n),

    "Weibull (0.5, 1)": lambda n: weibull_min(c=1, scale=0.5).rvs(size=n),
    "Weibull (2, 1)":   lambda n: weibull_min(c=1, scale=2).rvs(size=n),

    "Egyenletes (0, 1)":   lambda n: uniform(loc=0, scale=1).rvs(size=n),

    "Beta (2, 2)":     lambda n: beta(a=2, b=2).rvs(size=n),
    "Beta (0.5, 2)":   lambda n: beta(a=0.5, b=2).rvs(size=n),
    "Beta (3, 1.5)":   lambda n: beta(a=3, b=1.5).rvs(size=n),
    "Beta (2, 1)":     lambda n: beta(a=2, b=1).rvs(size=n),
    
    "Keverék: 0.95*N(0,1) + 0.05*N(3,1)":  lambda n: mixture_normal(n, mu=3, sigma=1, eps=0.05),
    "Keverék: 0.9*N(0,1) + 0.1*N(3,1)":  lambda n: mixture_normal(n, mu=3, sigma=1, eps=0.10),
    "Keverék: 0.95*N(0,1) + 0.05*N(0,9)":  lambda n: mixture_normal(n, mu=0, sigma=3, eps=0.05),
    "Keverék: 0.9*N(0,1) + 0.1*N(0,9)":  lambda n: mixture_normal(n, mu=0, sigma=3, eps=0.10),
}

# Csoport beazonosítása
GROUP_MAP = {
    # (-∞,+∞) tartójú, szimmetrikus
    "Student-féle t-eloszlás (df=1)": "(-∞;+∞) szimmetrikus",
    "Student-féle t-eloszlás (df=3)": "(-∞;+∞) szimmetrikus",
    "Standard logisztikus": "(-∞;+∞) szimmetrikus",
    "Standard Laplace": "(-∞;+∞) szimmetrikus",

    # (-∞,+∞) tartójú, aszimmetrikus
    "Gumbel(0, 1)": "(-∞;+∞) aszimmetrikus",
    "Gumbel(0, 2)": "(-∞;+∞) aszimmetrikus",
    "Gumbel(0, 0.5)": "(-∞;+∞) aszimmetrikus",

    # (0,+∞) tartójú
    "Exponenciális (1)": "(0;+∞)",
    "Gamma (2, 1)": "(0;+∞)",
    "Gamma (0.5, 1)": "(0;+∞)",
    "Lognormális (0, 1)": "(0;+∞)",
    "Lognormális (0, 2)": "(0;+∞)",
    "Weibull (0.5, 1)": "(0;+∞)",
    "Weibull (2, 1)": "(0;+∞)",

    # (0,1) tartójú
    "Egyenletes (0, 1)": "(0;1)",
    "Beta (2, 2)": "(0;1)",
    "Beta (0.5, 2)": "(0;1)",
    "Beta (3, 1.5)": "(0;1)",
    "Beta (2, 1)": "(0;1)",

    # Kevert eloszlások (normál keverékek)
    "Keverék: 0.95*N(0,1) + 0.05*N(3,1)": "kevert (normál keverék)",
    "Keverék: 0.9*N(0,1) + 0.1*N(3,1)": "kevert (normál keverék)",
    "Keverék: 0.95*N(0,1) + 0.05*N(0,9)": "kevert (normál keverék)",
    "Keverék: 0.9*N(0,1) + 0.1*N(0,9)": "kevert (normál keverék)",
}

# Csoport visszaadása
def classify_group(name: str) -> str:
    if name in GROUP_MAP:
        return GROUP_MAP[name]
    

#%% Dataframe létrehozása

# Függvény létrehozása, ami adott eloszlások és elemszám alapján kiszámolja a négy próba p-értékét
def p_value(expr1, expr2, n):

    sample1 = DIST[expr1](n)
    sample2 = DIST[expr2](n)

    # Sturges-féle osztályköz (k)
    k = int(1 + np.log2(n))
    bins = np.linspace(min(sample1.min(), sample2.min()),
                       max(sample1.max(), sample2.max()),
                       k + 1)

    c1, _ = np.histogram(sample1, bins=bins)
    c2, _ = np.histogram(sample2, bins=bins)
    mask = (c1 + c2) > 0
    dropped_bins_st = np.sum(~mask)
    table_st = np.vstack([c1[mask], c2[mask]])
    chi2_stat_st, chi_p_st, dof_st, exp_st = chi2_contingency(table_st, correction=False)

    # Decilis alapú bontás
    combined = np.concatenate([sample1, sample2])
    quantiles = np.percentile(combined, np.linspace(0, 100, 11))

    c1d, _ = np.histogram(sample1, bins=quantiles)
    c2d, _ = np.histogram(sample2, bins=quantiles)
    table_dec = np.vstack([c1d, c2d])
    chi2_stat_dec, chi_p_dec, dof_dec, exp_dec = chi2_contingency(table_dec, correction=False)
    test2 = (exp_dec < 5).mean() <= 0.2
    
    # Ha túl sok kicsi várt gyakoriság, kevesebb kategória
    if not test2:
        q5 = np.percentile(combined, np.linspace(0, 100, 6))  # 0%,20%,40%,60%,80%,100%
        c1q5, _ = np.histogram(sample1, bins=q5)
        c2q5, _ = np.histogram(sample2, bins=q5)
        table_q5 = np.vstack([c1q5, c2q5])
        chi2_stat_dec, chi_p_dec, dof_dec, exp_dec = chi2_contingency(table_q5, correction=False)

    # Kolmogorov-Smirnov és Kruskal-Wallis próba
    ks_stat, ks_p = ks_2samp(sample1, sample2)
    kw_stat, kw_p = kruskal(sample1, sample2)

    return {
        "Khi-négyzet (Sturges)": chi_p_st,
        "Khi-négyzet (decilis)": chi_p_dec,
        "Kolmogorov–Smirnov": ks_p,
        "Kruskal–Wallis": kw_p,
        "Dropped bins (Sturges)": int(dropped_bins_st)
    }


results = []

# Elemszámok listája
sample_sizes = list(range(10, 501, 10)) + list(range(550, 1001, 50))

# Gépet ébren tartó beállítás Windows-on (hosszú futáshoz)
import ctypes
ES_CONTINUOUS      = 0x80000000
ES_SYSTEM_REQUIRED = 0x00000001
ctypes.windll.kernel32.SetThreadExecutionState(ES_CONTINUOUS | ES_SYSTEM_REQUIRED)

# Szimuláció: minden elemszám, minden eloszlás-pár, 100 véletlen mag
for n in sample_sizes:
    print(f"Elemszám: {n}")
    for i, (name1, name2) in enumerate(product(DIST.keys(), DIST.keys())):
        for seed in range(100):
            np.random.seed(seed)
            res = p_value(name1, name2, n)

            test1 = res.get("Dropped bins (Sturges)")/n

            for test_name, p in res.items():
                results.append({
                    "Eloszlás 1": name1,
                    "Eloszlás 2": name2,
                    "Elemszám": n,
                    "Próba": test_name,
                    "P-Érték": p,
                    "Random mag": seed,
                    "Test1": test1
                })

# Eredeti energiagazdálkodás visszaállítása
ctypes.windll.kernel32.SetThreadExecutionState(ES_CONTINUOUS)

# Nyers eredmények DataFrame-ben
df = pd.DataFrame(results)

head=df.head(100)

# Döntés 5% és 1% szinten
df['döntés 5'] = df['P-Érték'] < 0.05
df['döntés 1'] = df['P-Érték'] < 0.01

# Csoporthozzárendelés
df["Csoport 1"] = df["Eloszlás 1"].map(classify_group)
df["Csoport 2"] = df["Eloszlás 2"].map(classify_group)

df[df.Test1==0.2]

# Aggregálás a 100 különböző random mag mentén
df_summary = (
    df
    .groupby(["Eloszlás 1", "Eloszlás 2", "Elemszám", "Próba", "Csoport 1", "Csoport 2"], as_index=False)
    .agg({
        "döntés 5": "mean",    
        "döntés 1": "mean",
        "P-Érték": ["mean", "min", "max"]
    })
)

df_summary.columns = [
    " ".join(col).strip() if isinstance(col, tuple) else col
    for col in df_summary.columns
]

# Oszlopok elnevezése
df_summary = df_summary.rename(columns={
    "döntés 5 mean": "Döntés 5 arány",
    "döntés 1 mean": "Döntés 1 arány",
    "P-Érték mean": "P-Érték átlag",
    "P-Érték min": "P-Érték minimum",
    "P-Érték max": "P-Érték maximum"
})


head_summary = df_summary.head(100)

df.info()
df_summary.info()

# CSV mentések
df.to_csv("eredmenyek_nyers.csv", index=False, encoding="utf-8-sig")

df_summary = df_summary[df_summary["Próba"] != "Dropped bins (Sturges)"]

df_summary.to_csv("eredmenyek_osszefoglalo.csv", index=False, encoding="utf-8-sig")

#%% Az eloszlások elméleti tulajdonságai

# Sűrűségfüggvények létrehozása
DIST_PDF = {
    # (-∞,+∞) tartójú, szimmetrikus
    "Student-féle t-eloszlás (df=1)":                     lambda x: t(df=1).pdf(x),
    "Student-féle t-eloszlás (df=3)":                     lambda x: t(df=3).pdf(x),
    "Standard logisztikus":    lambda x: logistic(loc=0, scale=1).pdf(x),
    "Standard Laplace":     lambda x: laplace(loc=0, scale=1).pdf(x),

    # (-∞,+∞) tartójú, aszimmetrikus
    "Gumbel(0, 1)":    lambda x: gumbel_r(loc=0, scale=1).pdf(x),
    "Gumbel(0, 2)":    lambda x: gumbel_r(loc=0, scale=2).pdf(x),
    "Gumbel(0, 0.5)":  lambda x: gumbel_r(loc=0, scale=0.5).pdf(x),

    # (0,+∞) tartójú
    "Exponenciális (1)":              lambda x: expon(scale=1).pdf(x),
    "Gamma (2, 1)":         lambda x: gamma(a=2, scale=1).pdf(x),
    "Gamma (0.5, 1)":       lambda x: gamma(a=0.5, scale=1).pdf(x),
    "Lognormális (0, 1)":  lambda x: lognorm(s=1, scale=np.exp(0)).pdf(x),
    "Lognormális (0, 2)":  lambda x: lognorm(s=2, scale=np.exp(0)).pdf(x),
    "Weibull (0.5, 1)": lambda x: weibull_min(c=1, scale=0.5).pdf(x),
    "Weibull (2, 1)":   lambda x: weibull_min(c=1, scale=2).pdf(x),

    # (0,1) tartójú
    "Egyenletes (0, 1)":     lambda x: uniform(loc=0, scale=1).pdf(x),
    "Beta (2, 2)":              lambda x: beta(a=2, b=2).pdf(x),
    "Beta (0.5, 2)":            lambda x: beta(a=0.5, b=2).pdf(x),
    "Beta (3, 1.5)":            lambda x: beta(a=3, b=1.5).pdf(x),
    "Beta (2, 1)":              lambda x: beta(a=2, b=1).pdf(x),

    # Keverék sűrűségek
    "Keverék: 0.95*N(0,1) + 0.05*N(3,1)":  lambda x: 0.95*norm.pdf(x, 0, 1) + 0.05*norm.pdf(x, 3, 1),
    "Keverék: 0.9*N(0,1) + 0.1*N(3,1)":  lambda x: 0.90*norm.pdf(x, 0, 1) + 0.10*norm.pdf(x, 3, 1),

    "Keverék: 0.95*N(0,1) + 0.05*N(0,9)":  lambda x: 0.95*norm.pdf(x, 0, 1) + 0.05*norm.pdf(x, 0, 3),
    "Keverék: 0.9*N(0,1) + 0.1*N(0,9)":  lambda x: 0.90*norm.pdf(x, 0, 1) + 0.10*norm.pdf(x, 0, 3),
}

# Átfedési együttható (OVL) numerikus integrálással
def overlap_coefficient(f, g, lower=-np.inf, upper=np.inf):
    integrand = lambda x: np.minimum(f(x), g(x))
    return quad(integrand, lower, upper, limit=200)[0]


rows = []
keys = list(DIST_PDF.keys())

# Összes eloszláspár kiválasztása
for i, k1 in enumerate(keys):
    for j, k2 in enumerate(keys):
        if j <= i:
            continue
        f = DIST_PDF[k1]
        g = DIST_PDF[k2]
        ov = overlap_coefficient(f, g)
        rows.append([k1, k2, ov])

df_elemzes_ertekek = pd.DataFrame(rows, columns=["Eloszlás 1", "Eloszlás 2", "Átfedés (OVL)"])

# Csoport hozzárendelése
df_elemzes_ertekek["Csoport 1"] = df_elemzes_ertekek["Eloszlás 1"].map(classify_group)
df_elemzes_ertekek["Csoport 2"] = df_elemzes_ertekek["Eloszlás 2"].map(classify_group)

# Eloszlások elméleti tulajdonságainak beolvasása
tulajdonsagok = pd.read_excel("eloszlas_tulajdonsagok.xlsx")

key = tulajdonsagok["Eloszlás"]
median_map   = dict(zip(key, tulajdonsagok["Medián"]))
range_map    = dict(zip(key, tulajdonsagok["Tartomány"]))
skew_map    = dict(zip(key, tulajdonsagok["Ferdeség"]))
kurtosis_map    = dict(zip(key, tulajdonsagok["Csúcsosság"]))
modusz_map    = dict(zip(key, tulajdonsagok["Módusz"]))


eloszlas1 = df_elemzes_ertekek["Eloszlás 1"]
eloszlas2 = df_elemzes_ertekek["Eloszlás 2"]

# Eloszlás 1 jellemzői
df_elemzes_ertekek["Eloszlás 1 medián"]     = eloszlas1.map(median_map)
df_elemzes_ertekek["Eloszlás 1 ferdeség"]   = eloszlas1.map(skew_map)
df_elemzes_ertekek["Eloszlás 1 tartomány"]  = eloszlas1.map(range_map)
df_elemzes_ertekek["Eloszlás 1 csúcsosság"]   = eloszlas1.map(kurtosis_map)
df_elemzes_ertekek["Eloszlás 1 módusz"]  = eloszlas1.map(modusz_map)


# Eloszlás 2 jellemzői
df_elemzes_ertekek["Eloszlás 2 medián"]     = eloszlas2.map(median_map)
df_elemzes_ertekek["Eloszlás 2 ferdeség"]   = eloszlas2.map(skew_map)
df_elemzes_ertekek["Eloszlás 2 tartomány"]  = eloszlas2.map(range_map)
df_elemzes_ertekek["Eloszlás 2 csúcsosság"]   = eloszlas2.map(kurtosis_map)
df_elemzes_ertekek["Eloszlás 2 módusz"]  = eloszlas2.map(modusz_map)

# Mediánkülönbség abszolút értékben
df_elemzes_ertekek["Mediánkülönbség"] = (
    (df_elemzes_ertekek["Eloszlás 1 medián"] - df_elemzes_ertekek["Eloszlás 2 medián"]).abs()
)

# Tartomány- és csoportazonosság
df_elemzes_ertekek["Tartomány egyezik"] = (
    df_elemzes_ertekek["Eloszlás 1 tartomány"] == df_elemzes_ertekek["Eloszlás 2 tartomány"]
)

df_elemzes_ertekek["Csoport egyezik"] = (
    df_elemzes_ertekek["Csoport 1"] == df_elemzes_ertekek["Csoport 2"]
)

# Különbség számítása (vagy "nincs", ha nem numerikus)
def diff_or_none(x, y):
    if isinstance(x, (int, float, np.number)) and isinstance(y, (int, float, np.number)):
        return abs(x - y)
    else:
        return "nincs"

# Ferdeségkülönbség abszolút értékben
df_elemzes_ertekek["Ferdeségkülönbség"] = df_elemzes_ertekek.apply(
    lambda row: diff_or_none(row["Eloszlás 1 ferdeség"], row["Eloszlás 2 ferdeség"]),
    axis=1
)

# Csúcsosságkülönbség abszolút értékben
df_elemzes_ertekek["Csúcsosságkülönbség"] = df_elemzes_ertekek.apply(
    lambda row: diff_or_none(row["Eloszlás 1 csúcsosság"], row["Eloszlás 2 csúcsosság"]),
    axis=1
)

# Móduszkülönbség abszolút értékben
df_elemzes_ertekek["Móduszkülönbség"] = df_elemzes_ertekek.apply(
    lambda row: diff_or_none(row["Eloszlás 1 módusz"], row["Eloszlás 2 módusz"]),
    axis=1
)

df_nincs = df_elemzes_ertekek[df_elemzes_ertekek.eq("nincs").any(axis=1)]

# Elemzéshez szükséges oszlopok kiválasztása
df_elemzes = df_elemzes_ertekek.copy()
df_elemzes = df_elemzes[['Eloszlás 1', 'Eloszlás 2', 'Csoport 1', 'Csoport 2', 'Átfedés (OVL)', 'Mediánkülönbség', 'Tartomány egyezik', 'Csoport egyezik', 'Ferdeségkülönbség', 'Csúcsosságkülönbség', 'Móduszkülönbség']]

# P-értékek n=50-es elemszámra
p_ertekek = df_summary[['Eloszlás 1', 'Eloszlás 2', 'Elemszám', 'Próba', 'P-Érték átlag']]
p_ertekek = p_ertekek[p_ertekek["Elemszám"] == 50]
p_ertekek = p_ertekek[p_ertekek["Próba"] != "Dropped bins (Sturges)"]


p_wide = (
    p_ertekek
    .pivot_table(
        index=["Eloszlás 1", "Eloszlás 2"],
        columns="Próba",
        values="P-Érték átlag"
    )
    .reset_index()
)

# DataFrame létrehozása: eloszlástulajdonságok és p-értékek
df_elemzes = df_elemzes.merge(
    p_wide,
    on=["Eloszlás 1", "Eloszlás 2"],
    how="left"
)

# Export Excelbe
df_elemzes.to_excel("elemzes.xlsx")


#%% Sturges kontingenciatáblájának ellenőrzése a lognormális és a t-eloszlás esetén

# Minták generálása
sample1 = lognorm(s=2, scale=np.exp(0)).rvs(size=100, random_state=118)
sample2 = t(df=1).rvs(size=100, random_state=118)

n = 100
k = int(1 + np.log2(n))

# Közös tartomány
min_val = min(sample1.min(), sample2.min())
max_val = max(sample1.max(), sample2.max())

bins = np.linspace(min_val, max_val, k + 1)

# Gyakoriságok hisztogram alapján
c1, _ = np.histogram(sample1, bins=bins)
c2, _ = np.histogram(sample2, bins=bins)

# Kontingenciatábla DataFrame-ben
df_contingency_sturges = pd.DataFrame({
    "bin_start": bins[:-1],
    "bin_end":   bins[1:],
    "count_sample1": c1,
    "count_sample2": c2
})


df_contingency_sturges

