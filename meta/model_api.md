# input

* add project (+config!)
* add/remove/update file/dir
* update text @pos (value)

#output

## syntax highlighting

* get tokens @pos @length

* spi for semantic handlers (filter token list, replace with enhanced kind)

## outline / content assist

* get Type of Scope (matching Pattern)
* get docs of Item

* spi for doc handlers

## search

* get refs of Item
* get defs of Item

advanced:
* get undefined (all with defs==0)
* get unused (all with refs==0)

* spi for filters

## validation

* get markers for Container

* spi to add validators: syntax, xrefs, etc

## folding

## pretty-print/indent

## partitioning

# others

These don't need the model machinery?  

## templates

## quick fixes

## autoedit

## wizards?

# interface with Java

* do we cache anything on the java side? preliminary: only values for currently visible/active stuff
* java objects have no state, are just values
* use events extensively (both directions), so that we don't have to keep track of hundreds of listeners
* we need generic implementations of eclipse functionality that listens/sends events (see xtext?)

CAN WE DO THIS WITHOUT STARTING AFRESH?

# SPI in Erlang

How to implement plugin functionality in Erlang? More precisely, how we register and find all modules implementing a certain extension point (to use Eclipse terminology)


