build:
	elm make src/Main.elm --output=app.js

build-production:
	elm make src/Main.elm --optimize --output=app.js

make:
	elm make src/Main.elm

reactor:
	elm reactor

open:
	open index.html

serve:
	http-server -p 3000 --cors
