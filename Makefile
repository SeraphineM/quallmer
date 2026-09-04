.PHONY: install articles article deploy-articles site readme

# Knit README.md from README.Rmd
readme:
	Rscript -e "rmarkdown::render('README.Rmd', quiet = TRUE)"

# Install the checkout. pkgdown knits articles in a fresh process against
# the installed package, not the source tree, so an article that reads
# inst/extdata through system.file() or prints an object sees whatever
# version is installed; a stale one gives stale or missing objects.
install:
	R CMD INSTALL --no-multiarch .

# Build all articles locally (with updated README)
articles: readme install
	Rscript -e "pkgdown::build_articles()"

# Build a specific article
# Usage: make article NAME=pkgdown/getting-started/workflow
article: install
	Rscript -e "pkgdown::build_article('$(NAME)')"

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
site: readme install
	Rscript -e "pkgdown::build_site()"
