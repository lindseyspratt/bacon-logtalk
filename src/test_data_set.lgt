:- object(test_data_set).

:- public([test_data_set/6]).

test_data_set(linear, 0.001, 0.001, 0.01, [[1,2],[3,4],[2,3]], [x,y]).

test_data_set(square, 0.001, 0.001, 0.01, [[1,1],[2,4],[3,9]], [x,y]).

test_data_set(square_cube, 0.001, 0.001, 0.01, [[1,1],[4,8],[9,27]], [x,y]).

/* Following is idealized data for the law of uniform acceleration by Galileo Galilei. (D/T^2 = k). From page 83. */
  test_data_set(uniform_acceleration, 0.001, 0.001, 0.01,
[[0.1, 0.098],
[0.2,0.392],
[0.3,0.882],
[0.4,1.568],
[0.5,2.45],
[0.6,3.528]], [t,d]).

/* Following is Boyle's actual data for determining the gas law. (P*V = k). From page 82.  This needs the imprecision limit for infer_constants (actually in analyze_constant_evaluations) to be set to  about 0.15 */
  test_data_set(original_boyles_gas_law, 0.15, 0.001, 0.01,
[[1,29.75],
[1.5,19.125],
[2,14.375],
[3,9.5],
[4,7.125],
[5,5.625],
[6,4.875],
[7,4.25],
[8,3.75],
[9,3.375],
[10,3],
[12,2.625],
[14,2.25],
[16,2],
[18,1.875],
[20,1.75],
[24,1.5],
[28,1.375],
[32,1.25]], [p,v]).
/* Ohm's original data for electrical circuits. p. 84.  len*imp = -20.14993361510822223*len + 7245.18359387258871.  This needs infer_linear linearity limit of 0.1 (using other_linear_error). */
test_data_set(ohm1, 0.001, 0.1, 0.01,
[[2.0, 326.75],
[4.0, 300.75],
[6.0, 277.75],
[10.0, 238.25],
[18.0, 190.75],
[34.0, 134.5],
[66.0, 83.25],
[130, 48.5]], [len, imp]).

/* distance^3 / period ^ 2 = 54.21588927399393274.  This needs the linearity limit at 0.01 (or lower) (the data is exact), and constant limit of ~ 0.1 (due to digital calculation errors?).
*/
test_data_set(borelli, 0.1, 0.01, 0.01,
[[5.67, 1.769],
[8.67, 3.571],
[14.00, 7.155],
[24.67, 16.689]], [distance, period]).

/* a/moles = 8.347222222222222214.
b/moles = 2271.092592592592593.
pressure*vol = a*temp + b.
pressure*vol = (8.347222222222222214*moles)*temp + (2271.092592592592593*moles).
The data is exact, so low precision limits are needed (linearity (other_linear_error) limit of 0.01).
*/
test_data_set(ideal_gas, 0.001, 0.001, 0.01,
[1 - [10 - [[1000,2.354],
[2000,1.177],
[3000,0.785]],
20 -[[1000,2.438],
[2000,1.218],
[3000,0.813]],
30 - [[1000,2.521],
[2000,1.265],
[3000,0.84]]],
2 - [10 - [[1000,4.709],
[2000,2.354],
[3000,1.57]],
20 - [[1000,4.876],
[2000,2.438],
[3000,1.625]],
30 - [[1000,5.042],
[2000,2.521],
[3000,1.681]]],
3 - [10 - [[1000,7.064],
[2000,3.532],
[3000,2.355]],
20 - [[1000,7.313],
[2000,3.657],
[3000,2.438]],
30 - [[1000,7.563],
[2000,3.782],
[3000,2.521]]]], [moles, temp, pressure, vol]).
/* Following is synthetic data for Ohm's law, p. 104. 

Law: diam*diam*temp/intercept(current, current*len) = 0.1375972598971769999

The data for iron-100-0.01 analyzes a constant for LI, where for all other data sets at this level a linear relationship between I and LI is appropriate.  So, this data is shifted to not be first.
This data set is correctly analyzed with the constant "error" measure limit (in infer_constants) set to 0.001. A spurious relationship is recognized if this limit is set to 0.005 (or higher).
The linearity (other_linear_error) limit should be set to ~0.01, setting it lower (0.001, for instance), prevents the recognition of linearity for current versus len*current.
*/
test_data_set(ohm2, 0.001, 0.01, 0.01,
[iron - [100 - [0.02 - [[0.5, 0.57870], [1.0, 0.29002], [1.5, 0.19349]],
0.01 -[[0.5, 0.14518], [1.0, 0.07263], [1.5, 0.04843]],
0.03 - [[0.5, 1.29459], [1.0, 0.65066], [1.5, 0.43453]]],
120 - [0.01 -[[0.5, 0.17421], [1.0, 0.08715], [1.5, 0.058117]],
0.02 - [[0.5, 0.69444], [1.0, 0.34803], [1.5, 0.23220]],
0.03 - [[0.5, 1.55350], [1.0, 0.78080], [1.5, 0.52144]]],
140 - [0.01 -[[0.5, 0.20325], [1.0, 0.10169], [1.5, 0.06780]],
0.02 - [[0.5, 0.81019], [1.0, 0.40603], [1.5, 0.27090]],
0.03 - [[0.5, 1.81243], [1.0, 0.91093], [1.5, 0.60834]]]
],
silver - [100 - [0.01 -[[0.5, 0.1453], [1.0, 0.0727], [1.5, 0.0484]],
0.02 - [[0.5, 0.5809], [1.0, 0.2906], [1.5, 0.1937]],
0.03 - [[0.5, 1.3059], [1.0, 0.6535], [1.5, 0.4358]]],
120 - [0.01 -[[0.5, 0.1744], [1.0, 0.0872], [1.5, 0.05814]],
0.02 - [[0.5, 0.6971], [1.0, 0.3487], [1.5, 0.2325]],
0.03 - [[0.5, 1.5671], [1.0, 0.7842], [1.5, 0.5230]]],
140 - [0.01 -[[0.5, 0.2034], [1.0, 0.1017], [1.5, 0.0678]],
0.02 - [[0.5, 0.8133], [1.0, 0.4068], [1.5, 0.2712]],
0.03 - [[0.5, 1.8283], [1.0, 0.9149], [1.5, 0.6101]]]
]],
[metal,temp,diam,len,current]).

/*
z = a*y + b
x*x / a = 2
x*x / b = 2
a,b = 0.5, x = 1
a,b= 2, x = 4
a,b = 4.5, x = 9
*/
test_data_set(square_slope, 0.001, 0.001, 0.01,
[1-[[1,1],[2,1.5],[3,2]],2-[[1,4],[2,6],[3,8]],3-[[1,9],[2,13.5],[3,18]]], [x,y,z]).

test_data_set(sequence, 0.001, 0.001, 0.01,
[[1,12], [2,56], [3,182], [4,462], [5,992]], [p,n]).

/*
com.highfleet.pique,169,29,55,43,0.17,0.44,0.39,1
com.highfleet.pique.builtin,52,2,10,23,0.04,0.7,0.26,1
com.highfleet.pique.builtin.admin,53,3,2,29,0.06,0.94,0.01,1
*/
/*
test_data_set(jdepend, 0.001, 0.001, 0.01,
[
[1,6,24,21],
[0,2,2,5],
[0,3,4,4],
[1,0,10,3],
[3,2,3,4],
[0,1,20,5],
[0,4,13,3],
[7,1,7,2],
[15,3,17,3],
[18,6,24,2]
],
[abstract_class_count, ca, class_count, ce]).
*/
/*
Following calculations used in defining data for ohm2.  The ohm2 procedure is used to generate the data in the ohm2 test_data_set.

LI = aI + b
I(L-a) = b
I = b/(L-a)
a /(D*D) = K
I = b/(L - K*D*D)
T*D*D/b = 0.1376
I = T*D*D/(0.1376*(L - K*D*D))
*/

ohm2(Metal, T, D, [0.5-I1, 1-I2, 1.5-I3]) :-
          ohm2(Metal, T, D, 0.5, I1),
          ohm2(Metal, T, D, 1, I2),
          ohm2(Metal, T, D, 1.5, I3).

ohm2(Metal, T, D, L, I) :-
          (Metal = iron -> K = -5.8140
          ;
          Metal = silver -> K = -0.9477
          ;
          throw(unknown_metal(Metal))
          ),
          I is T*D*D/(0.1376*(L - K*D*D)).


:- end_object.
