
js:
	NODE_PATH=./output browserify -o VerneLang.js \
			  -r ./output/VerneLang:VerneLang \
			  -r ./output/Data.Either:Data.Either

