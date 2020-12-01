# bacon-logtalk
The bacon system is based on _Scientific Discovery: Computational Explorations of the Creative Processes_
by Pat Langley, Herbert A. Simon, Gary L. Bradshaw, and Jan M. Zytkow, 1987, MIT Press. 
Langley et. al. created a system they called bacon and discussed it in _Scientific Discovery_.
This bacon implementation is my attempt at a reconstruction in Prolog of that system based only on reading their book.

bacon analyzes a table of data to determine a law that describes it, a relationship among the values in the rows 
that is the same for all of the rows in the table.

The general topic is discussed at https://plato.stanford.edu/entries/scientific-discovery/:

I originally implemented this version of bacon in 1990's in MacProlog32, then ported that to XGP. 
This is a port to Logtalk for any Prolog backend.

There are several examples defined in test_data_set.lgt. You can list their names by 'setof(ID, bacon_loader::bacon_test_id(ID), IDs)'.

To run the 'linear' example with GNU Prolog backend:

````
$ cd <bacon_project_dir>

$ gplgt

| ?- logtalk_load(loader).

| ?- bacon_loader::set_test_parameters(linear).                 

yes
| ?- bacon_loader::run_test(linear).                 

Test: linear
     SPECIFIED PARAMETERS:
     Max. number of search variables = 10
     Constant tolerance = 0.001 (recommended)
     Linear tolerance = 0.001 (recommended)
     Proportional tolerance = 0.01 (recommended)

Start to determine regularities.
Determine regularities for: [x,y]
[[1,2],[3,4],[2,3]]

slope(x,y) = 1.0
intercept(x,y) = 1.0
Time to find law: 0.0010000000000000009 seconds.

(1 ms) yes
| ?- 

````
The 'ideal_gas' example fails to find a law (sigh).

The 'borelli' example works with GNU Prolog backend but fails with the SWI-Prolog 
backend (swipl terminates without an error message).

The bacon analysis is very sensitive to the tolerance parameters.
This sensitivity is chaotic.


