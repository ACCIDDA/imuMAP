# imugap.maps

> ⚠️ **Under construction.** The scope of this package is still being figured
> out. The name in the DESCRIPTION is `imugap.maps`; the repo is
> `imugap-map`. The README will get more specific as the surface area
> stabilizes.

A sibling R package to [`imuGAP`](https://github.com/ACCIDDA/imuGAP) that
holds tools which depend on imuGAP but don't belong inside the modeling
package itself. The DESCRIPTION calls it "CLI and Geospatial Tools for
imuGAP" — the CLI half is real today, the geospatial half is aspirational.

## What's here today

A thin wrapper around `imuGAP::imuGAP()` packaged as a shell CLI:

- `install_cli()` — drops a symlink in `~/.local/bin` so `imugap` is on
  PATH.
- `inst/scripts/imugap.R` — the script the symlink points at. Reads
  `observations.{csv,rds}`, `populations.{csv,rds}`, `locations.{csv,rds}`
  from an input directory, runs imuGAP's canonicalize + sampler, writes
  `fit.rds` (a raw stanfit) to the output directory.

Usage:

```sh
imugap input/ output/
```

That's it. No post-processing, no aggregation, no geospatial functions.

## What might land here later

A few directions have been discussed; none are committed:

- Post-processing helpers that turn a stanfit into the tabular shapes
  downstream consumers (e.g. the [measles
  dashboard](https://github.com/ACCIDDA/measles-dashboard)) need.
- Geospatial joins between school-level model output and standardized
  location data from
  [USImmunityProfiles](https://github.com/ACCIDDA/USImmunityProfiles).
- CLI subcommands beyond "run the model."

We expect the scope to firm up as the dashboard's CSV consumption work
([measles-dashboard#20](https://github.com/ACCIDDA/measles-dashboard/issues/20))
and the broader release prep on imuGAP land.

## Installation

```r
# install.packages("remotes")
remotes::install_github("ACCIDDA/imugap-map")
```

Requires imuGAP, which is currently a GitHub-only dependency (declared
under `Remotes:` in `DESCRIPTION`). Once imuGAP is on CRAN that line can
go away.

## Repository status

- CI: in progress on [#7](https://github.com/ACCIDDA/imugap-map/pull/7).
  Once that merges, R-CMD-check + pkgdown run on every PR; `man/` is
  regenerated from roxygen comments in CI and is not tracked in git.
- Issues and discussion are tracked in
  [the issue tracker](https://github.com/ACCIDDA/imugap-map/issues).

## License

MIT. See [LICENSE](LICENSE.md).
