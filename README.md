# bacon-logtalk
The bacon system is based on "Scientific Discovery: Computational Explorations of the Creative Proesses"
by Pat Langley, Herbert A. Simon, Gary L. Bradshaw, and Jan M. Zytkow, 1987, MIT Press. 
Langley et. al. created a system they called bacon and discussed it in "Scientific Discovery".
This bacon implementation is my attempt at a reconstruction in Prolog of that system based only on reading their book.

bacon analyzes a table of data to determine a law that describes it, a relationship among the values in the rows 
that is the same for all of the rows in the table.

I originally implemented this version of bacon in 1990's in MacProlog32, then ported that to XGP. 
This is a port to Logtalk for either SWI-Prolog or GNU Prolog backends.

There are several examples defined in test_data_set.lgt. You can list their names by 'setof(ID, bacon_loader::bacon_test_id(ID), IDs)'.

To run the 'linear' example with SWI-Prolog backend:

$ cd <bacon_project_dir>

$ swilgt

?- logtalk_load(loader).

?- bacon_loader::set_test_parameters(linear).

?- bacon_loader::run_test(linear).


The 'ideal' example fails to find a law (sigh).

The 'borelli' example works with GNU Prolog backend but fails with the SWI-Prolog 
backend (swipl terminates without an error message).
