.PHONY: articles article deploy-articles deploy-article site readme

# Knit README.md from README.Rmd
readme:
	Rscript -e "rmarkdown::render('README.Rmd', quiet = TRUE)"

# pkgdown knits articles in a fresh process against an installed quallmer,
# not the source tree, so an article that reads inst/extdata through
# system.file() or prints an object sees whatever version is installed; a
# stale one gives stale or missing objects. The checkout is therefore
# installed into a throwaway library first, which R_LIBS puts ahead of the
# others for both the install and the render, so the developer's own
# installed quallmer is neither read nor overwritten. (pkgdown::build_site()
# does this itself, so `site` needs nothing of the kind.)
define with_checkout_installed
lib=$$(mktemp -d) && \
R_LIBS="$$lib" R CMD INSTALL --no-multiarch . && \
QUALLMER_LIVE_ARTICLES=1 R_LIBS="$$lib" Rscript -e "$(1)"; \
status=$$?; rm -rf "$$lib"; exit $$status
endef

# Explicit article builds enable live examples; other renders leave them unrun.
# Build all articles locally (with updated README)
articles: readme
	$(call with_checkout_installed,pkgdown::build_articles())

# Build a specific article
# Usage: make article NAME=pkgdown/getting-started/workflow
article:
	$(call with_checkout_installed,pkgdown::build_article('$(NAME)'))

# Build one article live and deploy it with whatever else is built
# Usage: make deploy-article NAME=pkgdown/tutorials/tools
deploy-article: article
	$(MAKE) deploy-articles

# Deploy articles and workshop materials to gh-pages without touching other content
deploy-articles: readme
	git worktree add --detach gh-pages-tmp gh-pages
	cp -r docs/articles/* gh-pages-tmp/articles/
	if [ -d docs/workshops ]; then mkdir -p gh-pages-tmp/workshops && cp -r docs/workshops/* gh-pages-tmp/workshops/; fi
	cd gh-pages-tmp && \
		git add -A && \
		git commit -m "Update articles and workshop materials" && \
		git push origin HEAD:gh-pages
	git worktree remove gh-pages-tmp

# Full local site build (with updated README)
site: readme
	QUALLMER_LIVE_ARTICLES=1 Rscript -e "pkgdown::build_site()"
