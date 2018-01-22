build:
	python setup.py sdist
	python setup.py bdist_wheel

release:
	pip install -U twine
	twine upload dist/*

.PHONY: build release
