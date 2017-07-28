publish: $(TARGET)
	npm run build-prod
	git stash
	git checkout gh-pages
	mv index-prod.html index.html
	git commit -am '[bot] Automatic update'
	git push origin gh-pages
	git checkout master
	git stash pop
