# R4AOP — Requirements

**Goal:** Give R users access to AOP-Wiki data through simple functions, so no SPARQL is needed.
**Data source:** AOP-Wiki REST API — https://aopwiki.rdf.bigcat-bioinformatics.org/api/docs (v1, JSON/CSV, paginated: max 1000 per call, `meta.total` gives the count)

**Scope:** Every function in Section 1 maps to one existing API endpoint. Nothing outside the API is in scope for v1.0.

**How to read this file**

* **ID** = unique label so GitHub issues and commits can refer to it. **FR** = Functional Requirement (what users can do), **TR** = Technical Requirement (how it works inside), **QR** = Quality/Release Requirement.
* **Priority** = when it is built: **M** = Must (v0.1–0.2), **S** = Should (v1.0), **C** = Could (later)

\---

## 1\. Functional requirements (what users can do)

### 1.1 AOPs

|ID|Function|API endpoint|Priority|
|-|-|-|-|
|FR-01|`aop\_list(q, status, taxon, ke, mie, ao, stressor, chemical, modified\_since)`|`GET /v1/aops`|M|
|FR-02|`aop\_get(aop\_id)`|`GET /v1/aops/{aop\_id}`|M|
|FR-03|`aop\_key\_events(aop\_id, role)`|`GET /v1/aops/{aop\_id}/key-events`|M|
|FR-04|`aop\_kers(aop\_id)`|`GET /v1/aops/{aop\_id}/kers`|M|
|FR-05|`aop\_stressors(aop\_id)`|`GET /v1/aops/{aop\_id}/stressors`|M|
|FR-06|`aop\_chemicals(aop\_id)`|`GET /v1/aops/{aop\_id}/chemicals`|M|
|FR-07|`aop\_genes(aop\_id, method)`|`GET /v1/aops/{aop\_id}/genes`|M|
|FR-08|`aop\_methods(aop\_id)`|`GET /v1/methods`|S|

### 1.2 Key Events (KEs)

|ID|Function|API endpoint|Priority|
|-|-|-|-|
|FR-09|`ke\_list(role, q, level, aop, taxon, organ, cell\_type, gene)`|`GET /v1/key-events`|M|
|FR-10|`mie\_list(...)` / `ao\_list(...)`|`GET /v1/mies`, `/v1/aos`|M|
|FR-11|`ke\_get(ke\_id)`|`GET /v1/key-events/{ke\_id}`|M|
|FR-12|`ke\_aops(ke\_id, role)`|`GET /v1/key-events/{ke\_id}/aops`|M|
|FR-13|`ke\_upstream(ke\_id, depth)` / `ke\_downstream(ke\_id, depth)`|`.../upstream`, `.../downstream` (depth 1–3)|M|
|FR-14|`ke\_adverse\_outcomes(ke\_id)` / `ke\_mies(ke\_id)`|`.../adverse-outcomes`, `.../molecular-initiating-events`|S|
|FR-15|`ke\_stressors(ke\_id)` / `ke\_chemicals(ke\_id)`|`.../stressors`, `.../chemicals`|S|

### 1.3 Key Event Relationships (KERs)

|ID|Function|API endpoint|Priority|
|-|-|-|-|
|FR-16|`ker\_list(aop, upstream, downstream, ke, gene)`|`GET /v1/kers`|M|
|FR-17|`ker\_get(ker\_id)`|`GET /v1/kers/{ker\_id}`|M|

### 1.4 Stressors and chemicals

|ID|Function|API endpoint|Priority|
|-|-|-|-|
|FR-18|`stressor\_list(q, aop, chemical, has\_chemical)` / `stressor\_get(id)`|`/v1/stressors`, `/v1/stressors/{id}`|S|
|FR-19|`chemical\_list(q, aop, stressor, inchikey, chebi, pubchem, ...)`|`GET /v1/chemicals`|S|
|FR-20|`chemical\_get(cas)`, `chemical\_aops(cas)`, `chemical\_key\_events(cas, role)`|`/v1/chemicals/{cas}/...`|S|
|FR-21|`chemical\_pathways(cas)` (WikiPathways)|`GET /v1/chemicals/{cas}/pathways`|S|

### 1.5 Genes, search, dataset

|ID|Function|API endpoint|Priority|
|-|-|-|-|
|FR-22|`gene\_list(q, aop, ke, method)` / `gene\_get(gene)`|`/v1/genes`, `/v1/genes/{gene}`|M|
|FR-23|`aop\_search(q, types)` (all entity types)|`GET /v1/search`|M|
|FR-24|`aop\_dataset\_info()` (version, license, counts)|`GET /v1/dataset`|S|

\---

## 2\. Technical requirements (how it works)

|ID|Requirement|Priority|
|-|-|-|
|TR-01|One internal request function (`httr2`) used by all functions: base URL, user agent, retries|M|
|TR-02|Automatic pagination for list endpoints (loop until `meta.total` is reached)|M|
|TR-03|All outputs returned as `tibble`s with consistent `snake\_case` column names|M|
|TR-04|Clear error messages for bad IDs (404), server errors (5xx) and no internet|M|
|TR-05|Input checks (e.g. `aop\_id` numeric, `depth` 1–3) before calling the API|M|
|TR-06|Base URL configurable via `options(R4AOP.base\_url = ...)` for testing / future versions|M|
|TR-07|Uses API version `/v1`; report `dataset\_version` so results are reproducible|S|
|TR-08|Optional caching of responses within a session (`memoise`)|S|
|TR-09|Minimal dependencies (`httr2`, `tibble`, `dplyr`; `igraph` only in Suggests)|S|

\---

## 3\. Quality and release requirements

|ID|Requirement|Priority|
|-|-|-|
|QR-01|Every exported function documented with `roxygen2`, including a runnable example|M|
|QR-02|Unit tests with `testthat`; API responses mocked (`httptest2`) so tests pass offline|M|
|QR-03|`R CMD check` passes with 0 errors, 0 warnings, 0 notes|M|
|QR-04|GitHub Actions: automatic check on Windows, macOS, Linux|S|
|QR-05|Test coverage ≥ 80% (`covr`)|S|
|QR-06|Vignette with a real use case (e.g. find AOPs for a chemical → their KEs → linked genes)|S|
|QR-07|pkgdown documentation website|S|
|QR-08|License: GPL-3 (code); cite AOP-Wiki data (CC BY-SA 4.0); `CITATION` file; Zenodo DOI|S|
|QR-09|Submission to CRAN|C|

\---

## 4\. Milestones

|Version|Content|
|-|-|
|**v0.1**|TR-01 to TR-06, FR-01 to FR-07, QR-01 to QR-03|
|**v0.2**|All remaining **M** and **S** FRs|
|**v1.0**|QR-04 to QR-08 → public release + application note|

\---

## 5\. Out of scope for v1.0 (possible later extensions, NOT in the API)

* Build an `igraph` network of an AOP (from FR-03 + FR-04 output)
* Plot AOP networks
* KE → gene sets ready for enrichment analysis

## 6\. Open questions (for API team)

1. How stable is `/v1`? Will endpoints or field names change?
2. Is a Python client planned (to avoid duplicate work)?
3. Is there a usage limit on the API?



