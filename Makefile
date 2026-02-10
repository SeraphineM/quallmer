.PHONY: articles deploy-articles site readme

# Knit README.md from README.Rmd
readme:
	Rscript -e "rmarkdown::render('README.Rmd', quiet = TRUE)"

# Build all articles locally (with updated README)
articles: readme
	Rscript -e "pkgdown::build_articles()"

# Build a specific article
# Usage: make article NAME=pkgdown/getting-started/workflow
article:
	Rscript -e "pkgdown::build_article('$(NAME)')"

# Deploy articles to gh-pages without touching other content
deploy-articles: readme
	git worktree add --detach gh-pages-tmp gh-pages
	cp -r docs/articles/* gh-pages-tmp/articles/
	cd gh-pages-tmp && \
		git add articles/ && \
		git commit -m "Update articles" && \
		git push origin HEAD:gh-pages
	git worktree remove gh-pages-tmp

# Full local site build (with updated README)
site: readme
	Rscript -e "pkgdown::build_site()"
