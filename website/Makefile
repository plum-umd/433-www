all:    build watch
	echo "Site running"

build:
	stack build
	stack exec site rebuild

watch:
	stack exec site watch

deploy:
	@scp -r _site/* llampro@junkfood.cs.umd.edu:/fs/www/class/spring2024/cmsc433
