R = R CMD BATCH --no-timing --quiet --vanilla

all: test
	mv sf12tests.Rout sf12tests.Rout.save
	mv sf36tests.Rout sf36tests.Rout.save

test:
	${R} sf12tests.R
	${R} sf36tests.R
