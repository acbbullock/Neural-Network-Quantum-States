# Paper draft

A teaching article on this program, aimed at the American Journal of Physics or the European Journal of Physics.
It derives stochastic reconfiguration from imaginary-time evolution, applies it to the transverse-field Ising chain,
and checks every result against the exact solution.

## Building

From the repository root, redraw the figures and print every number quoted in the paper:

```bash
Rscript paper/figures/make_figures.R
```

Then, from `paper/`, build the article and then the supplementary material, which takes the numbers of the
article's equations and sections from `main.aux`:

```bash
latexmk -pdf main.tex
latexmk -pdf supplementary.tex
```

## Format: American Journal of Physics

The manuscript follows AJP's instructions (https://www.aapt.org/Publications/AJPManFormat.cfm) and its sample file:

- REVTeX in the Phys. Rev. B preprint style (`aps,prb,preprint`). Replace `preprint` with `reprint` to see the
  two-column journal layout and estimate the printed length.
- Superscript endnotes after punctuation. `ajp.bst` is REVTeX's `apsrev4-2.bst` with article titles in quotes and
  first and last pages. Entries with four or more authors list the first author and "others", which prints "et al.".
- `anonymous.tex`, read by both documents, holds the review switch. `\anonymoustrue` (the default) leaves out the
  author, affiliation and acknowledgments, and replaces the citation of the code, which names the authors, with
  "Code reference anonymized for review." Set `\anonymousfalse` after conditional acceptance.
- AJP can't take BibTeX files for production: at that stage, paste the contents of `main.bbl` in place of the
  `\bibliographystyle` and `\bibliography` lines.

## Still to do before submission

1. Length: AJP asks that a main article normally fit in 6 journal pages. In the `reprint` layout this one is seven
   pages plus nine lines of references on an eighth, and about a quarter of page 7 is a gap left by REVTeX's
   column balancing. Moved to `supplementary.tex`: the pseudocode, the settings table, the sampling and SR-step
   details and the exact solution. Also done: references cut to 22, the field-sweep table dropped, Fig. 3 at column
   width, Table II at four fields for n = 64, two suggested problems dropped, Secs. I–III and VIII tightened. The
   rest is for the cover letter.
2. Before submitting: tell AJP about the AI assistance in the cover letter as well (the acknowledgments, hidden while
   `\anonymoustrue`, already say it), and check AIP Publishing's current AI policy.
