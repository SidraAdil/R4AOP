# R4AOP 0.99.0

* First Bioconductor submission.
* Added `ke_gene_sets()`, which returns the genes of each Key Event as a
  `GSEABase::GeneSetCollection`.
* Added `aop_summary()`, which returns the Key Events of an AOP in causal
  order, with the OECD status, taxonomic applicability and stressors.
* Added `stressor_aops()`, which lists the AOPs a stressor is used in,
  together with the adverse outcomes they end in.
* Added `ke_components()`, `aop_components()` and `aop_plot_components()`
  for Key Event Components and their ontology identifiers.
* Every result now carries the version of the AOP-Wiki dataset it came from.
* Relicensed under MIT.
