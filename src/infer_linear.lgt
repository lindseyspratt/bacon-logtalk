:- object(infer_linear).

:- public([infer_linear/8, check_linear/3, linear_approximation/11]).

infer_linear([TestValues|OtherTestValues], VariablesIn,
             linear_tolerances(LinearTolerance, ProportionalityTolerance),
             OldVariables, OldCalculations, ConsumedVariables, Values-ValueTail,
             NewVariables-VariablesTail) :-
          pairwise_expansion::pairwise_expansion(TestValues, ExpandedTestValues),
          check_linear(OtherTestValues, ExpandedTestValues, Evaluations),
          pairwise_expansion::pairwise_expansion(VariablesIn, ExpandedVariables),
          analyze_linear_evaluations(Evaluations, ExpandedVariables,
                                     LinearTolerance, ProportionalityTolerance,
                                     OldVariables, OldCalculations, ConsumedVariables, Values-ValueTail,
                                     NewVariables-VariablesTail).


analyze_linear_evaluations([], [], _, _, _, _, [], T1-T1, T2-T2).

analyze_linear_evaluations([Evaluation|OtherEvaluations],
                           [VariablePair|OtherVariablePairs], LinearTolerance, ProportionalityTolerance, 
                           OldVariables, OldCalculations, ConsumedVariables,
                           Values-ValuesTail, Names-VariablesTail
                          ) :-
          analyze_linear_evaluation(Evaluation, VariablePair, LinearTolerance, ProportionalityTolerance,
                                    OldVariables, OldCalculations,
                                    ConsumedVariables, ConsumedVariablesNext,
                                    Values - ValuesNext, Names - NamesNext),
          analyze_linear_evaluations(OtherEvaluations, OtherVariablePairs,
                                     LinearTolerance, ProportionalityTolerance, OldVariables,
                                     OldCalculations, ConsumedVariablesNext,
                                     ValuesNext-ValuesTail, NamesNext-VariablesTail).


analyze_linear_evaluation(nominal, _, _, _,_,_, ConsumedVariables, ConsumedVariables,
                          Values - Values, Names - Names).

analyze_linear_evaluation(linear(Count,Squares1,Squares2,Simple1, Simple2, Product),
                          VariablePair,
                          LinearTolerance, ProportionalityTolerance,
                          OldVariables, OldCalculations,
                          ConsumedVariables, ConsumedVariablesNext,
                          Values - ValuesNext, Names - NamesNext
                         ) :-
          linear_approximation(Count,Squares1, Squares2, Simple1, Simple2,
                               Product, Proportion, Proportionality,
                               Slope, Intercept, Linearity),
          VariablePair = XName-YName,
          (Proportionality =< ProportionalityTolerance, % 0.01,
           simplify::new_calculation(monotonic(increasing,XName,YName), OldVariables,
                           OldCalculations, NewCalc),
           \+ simplify::uninteresting_calculation(monotonic(increasing,XName,YName), NewCalc,
                                        OldCalculations)
             -> [Proportion|ValuesNext] = Values,
                [constant(monotonic(increasing, YName, XName))|NamesNext] = Names,
                [XName,YName|ConsumedVariablesNext] = ConsumedVariables,
                _ = Slope,
                _ = Intercept,
                _ = Linearity
           ;
           Linearity =< LinearTolerance, % 0.01, % 0.001,
           Intercept \= 0
             -> [Slope, Intercept|ValuesNext] = Values,
                [slope(XName,YName), intercept(XName,YName)|NamesNext] = Names,
                [XName,YName|ConsumedVariablesNext] = ConsumedVariables,
                _ = OldVariables,
                _ = OldCalculations,
                _ = Proportion,
                _ = Proportionality
           ;
            ValuesNext = Values,
            NamesNext = Names,
            ConsumedVariablesNext = ConsumedVariables,
            _ = OldVariables,
            _ = OldCalculations,
            _ = Proportion,
            _ = Proportionality,
            _ = Slope,
            _ = Intercept,
            _ = Linearity,
            _ = XName,
            _ = YName
          ).


check_linear([], Values, Values).

check_linear([TestValues|OtherTests], ValuesIn, ValuesOut) :-
          pairwise_expansion::pairwise_expansion(TestValues, ExpandedTestValues),
          check_test_linear(ExpandedTestValues, ValuesIn, ValuesNext),
          check_linear(OtherTests, ValuesNext, ValuesOut).


check_test_linear([], [], []).

check_test_linear([TestValue|OtherTestValues], [CheckValueIn|OtherCheckValuesIn],
                  [CheckValueOut|OtherCheckValuesOut]) :-
          accumulate_linear(TestValue, CheckValueIn, CheckValueOut),
          check_test_linear(OtherTestValues, OtherCheckValuesIn, OtherCheckValuesOut).


accumulate_linear(X-Y, CheckValueIn, CheckValueOut) :-
          (nominal_data::nominal_data(X), _ = Y
           ;
           nominal_data::nominal_data(Y), _ = X
          ) -> CheckValueOut = nominal,
               _ = CheckValueIn
          ;
          CheckValueIn = linear(Count,Squares1,Squares2,Simple1, Simple2, Product)
            -> NewCount is Count + 1,
               NewSquares1 is Squares1 + X*X,
               NewSquares2 is Squares2 + Y*Y,
               NewSimple1 is Simple1 + X,
               NewSimple2 is Simple2 + Y,
               NewProduct is Product + X*Y,
               CheckValueOut = linear(NewCount, NewSquares1, NewSquares2,
                                      NewSimple1, NewSimple2, NewProduct)
          ;
           CheckValueIn = X0-Y0,
           NewCount is 2,
           NewSquares1 is X0*X0 + X*X,
           NewSquares2 is Y0*Y0 + Y*Y,
           NewSimple1 is X0 + X,
           NewSimple2 is Y0 + Y,
           NewProduct is X0*Y0 + X*Y,
           CheckValueOut = linear(NewCount, NewSquares1, NewSquares2,
                                  NewSimple1, NewSimple2, NewProduct).


/* Following algorithm implements a least-squares fit for a linear equation of the form x2 = a*x1 + b.  The algorithm is adapted from Sedgewick's Algorithms, p. 552
General form for 2 parameter equation:
f(x) = c1*f1(x)+c2*f2(x)
Let f1(x) = 1 and f2(x) = x:
f(x) = c1 + c2*x.
This is the general form for a linear equation.
The general least-squares equations for 2 parameters:
c1*f1*f1 + c2*f1*f2 = f*f1
c1*f2*f1 + c2*f2*f2 = f*f2
where fi without a parameter is the vector (fi(x1), fi(x2), ..., fi(xN)) 
and f without a parameter is the vector (f(x1), f(x2),...,f(xN)),
c1 and c2 are 2 scalar constants (to be determined).
Substituting the linear equation definitions for f1 and f2:
f1*f1 = (1*1 + 1*1 + ... + 1*1) = N
f1*f2 = (1*x1 + 1*x2 + ... + 1*xN) = sum(xI, I=1 to N) = SimpleX
f2*f1 = f1*f2 = SimpleX
f2*f2 = (x1*x1 + ... + xN*xN) = sum(xI*xI, I=1 to N) = SquaresX
f*f1 = (f(x1)+...+f(xN)) = sum(f(xI), I = 1 to N) = SimpleY
f*f2 = (f(x1)*x1+...+f(xN)*xN) = sum(f(xI)*xI, I = 1 to N) = Product
c1 = Intercept
c2 = Slope

The solution for the parameters of the 2 parameter 2 equation system is:
(Multiply the first equation on both sides by (f2*f1 / f1*f1), and subtract the second equation from the first:)
=> c2 * (f1*f2 * (f2*f1 / f1*f1) - f2*f2) = f*f1* (f2*f1 / f1*f1) - f*f2
=> c2 * ((SimpleX*SimpleX / N) - SquaresX) = SimpleY * (SimpleX / N) - Product
=> c2 = ((SimpleY * SimpleX / N) - Product) / ((SimpleX*SimpleX / N) - SquaresX)
=> c2 = (SimpleY * SimpleX - N*Product / N) / ((SimpleX*SimpleX - N*SquaresX) / N)
=> c2 = (SimpleY * SimpleX - N*Product ) / (SimpleX*SimpleX - N*SquaresX)
(Solve the first equation for c1:)
=> c1 = (f*f1 - c2*f1*f2) / f1*f1
=> c1 = (SimpleY - c2*SimpleX) / N

For calculating if x and y are simply proportionally related (i.e. linearly related with a 0 slope):
f1 = (0,...,0)
=> 
c2 * SimpleX = SimpleY
c2 * SquaresX = Product
Either of these two equations is sufficient for determing a value for c2.  Using the second (smaller values):
c2 = SimpleY / SimpleX

To measure the degree to which the linear approximation describes the data, the standard deviation of the squares of the distances of the data points from the nearest points of the approximating line is compared with the standard deviation of the squares of the distances of the data points from the center point.  This normalizes the linearity measure of the data by the data's "single pointness".

The maximum allowable linearity error is the error distance if all of the data points were at the maximum tolerance of their distance from the inferred line.  Thus, the data matches the inferred line if the maximum tolerance error is greater than or equal to the actual error.

The data tolerance is (arbitrarily) specified to be ± 1%.  Thus, for example, the "real" value for a measured value of 100 lies in the range 99 to 101.

To calculate the data error distance:

The point which is closest to a data point (x,y) is x',y'. The distance line between the point and the discovered line is a line perpendicular to the discovered line:
y = -ax + c
y' = -ax' + c
=> c = y + ax
=> y' = -ax' + (y + ax)
The two lines share a point:
ax' + b = -ax' + (y + ax)
=> b = -2*ax' + y + ax
=> x' = (y + ax - b) / 2*a
=> y' = -a((y + ax - b) / 2*a) + (y + ax)
=> y' = ((-y - ax + b) / 2) + y + ax
=> y' = ((-y - ax + b) + 2*y + 2*ax)  / 2
=> y' = (y + ax + b) / 2

Thus the square of the distance between (x,y) and (x',y'):
d = (x-x')^2 + (y-y')^2
=> d = (x-x')^2 + (y-y')^2
=> d = (x-((y + ax - b) / 2*a))^2 + (y-((y + ax + b) / 2))^2
=> d = ((-y + ax + b) / 2*a))^2 + ((y - ax - b) / 2))^2
=> d = (-(y - ax - b) / 2*a))^2 + ((y - ax - b) / 2))^2
=> d = ((y - ax - b) / 2)^2 * (-1/a)^2 + ((y - ax - b) / 2)^2
=> d = ((y - ax - b) / 2)^2 * ((-1/a)^2 + 1)
=> d = ((y - ax - b) / 2)^2 * ((1/a)^2 + 1) [The (-1/a)^2 = (1/a)^2]
=> d = ((1/a)^2 + 1) * (1/4) * (y^2 - 2axy + (ax)^2 - 2by + 2abx + b^2)

The sum of the squares of the distances:
=> SumOfSquaresOfDistances = ((1+Slope*Slope)/Slope*Slope) * (1/4) * (SquaresY - 2*Slope*Product+ SlopeSquare*SquaresX - 2*Intercept*SimpleY - 2*Slope*Intercept*SimpleX + Intercept*Intercept*Count)

The data error is:
DataErrorDistance = sqrt(SumOfSquaresOfDistances / N).

*/

linear_approximation(Count,SquaresX,SquaresY,SimpleX, SimpleY,
                     Product, Proportion, Proportionality, Slope, Intercept, Linearity) :-
          (SimpleX =:= 0
             -> Proportionality = 1000,
                Proportion = undefined
           ;
            Proportion is SimpleY / SimpleX,
            other_linear_error(Count, SquaresX, SquaresY, SimpleX, SimpleY,
                               Product, Proportion, 0, Proportionality)
          ),
          SlopeDenominator is (SimpleX*SimpleX - Count*SquaresX),
          (SlopeDenominator =:= 0
             -> Slope = undefined,
                Intercept = undefined,
                Linearity = 1000,
                _ = SquaresY,
                _ = SimpleY,
                _ = Product
           ;
            Slope is (SimpleY * SimpleX - Count*Product ) / SlopeDenominator,
            Intercept is (SimpleY - Slope*SimpleX) / Count,
            ((Slope =:= 0 ; Intercept =:= 0)
              -> Linearity = 1000,
                 _ = SquaresY
            ;
             other_linear_error(Count, SquaresX, SquaresY, SimpleX,
                                SimpleY, Product, Slope, Intercept, Linearity)
            )
          ).


linear_error(Count, SquaresX, SquaresY, SimpleX, SimpleY,
             Product, Slope, Intercept, Linearity) :-
          tolerance_distance(Count, SquaresX, SquaresY, Tolerance),
          data_error_distance(Count, SquaresX, SquaresY, SimpleX, SimpleY,
                              Product, Slope, Intercept, DataErrorDistance),
          Linearity is DataErrorDistance / Tolerance.


/* Other measure:
The data is acceptably linear if the linearity error is (arbitrarily) less than or equal to 0.1.

The center data point is simply (SimpleX / N , SimpleY / N).

CenterSD = sqrt(sum((xI - xc)^2+(yI-yc)^2, I = 1 to N) / N)
=> CenterSD = sqrt((sum((xI - xc)^2, I = 1 to N)+sum((yI-yc)^2, I = 1 to N)) / N)
=> CenterSD = sqrt((sum((xI^2- 2*xI*xc+xc^2), I = 1 to N)+sum((yI^2-2*yI*yc+yc^2), I = 1 to N)) / N)
=> CenterSD = sqrt((sum(xI^2, I = 1 to N)- 2*xc*sum(xI, I = 1 to N) + N*xc^2)+(sum(yI^2, I = 1 to N)- 2*yc*sum(yI, I = 1 to N) + N*yc^2)) / N)
=> CenterSD = sqrt((SquaresX- 2*xc*SimpleX + N*xc^2)+(SquaresY- 2*yc*SimpleY + N*yc^2)) / N)
=> CenterSD = sqrt((SquaresX- (2*SimpleX^2)/N + (SimpleX^2)/N)+(SquaresY- (2*SimpleY^2)/N + (SimpleY^2)/N)) / N)
=> CenterSD = sqrt(((SquaresX- (SimpleX^2)/N)+(SquaresY- (SimpleY^2)/N)) / N)

Linearity = sqrt(SumOfSquaresOfDistances/N) / CenterSD
=>  Linearity = sqrt(SumOfSquaresOfDistances/N) / sqrt(((SquaresX- (SimpleX^2)/N)+(SquaresY- (SimpleY^2)/N)) / N)
=> Linearity = sqrt((SumOfSquaresOfDistances/N) / ((SquaresX- (SimpleX^2)/N)+(SquaresY- (SimpleY^2)/N)) / N)
=> Linearity = sqrt((SumOfSquaresOfDistances / ((SquaresX- (SimpleX^2)/N)+(SquaresY- (SimpleY^2)/N)))

*/

other_linear_error(Count, SquaresX, SquaresY, SimpleX, SimpleY, Product, Slope,
                   Intercept, Linearity) :-
          centroid_distance_squares(Count, SquaresX, SquaresY, SimpleX,
                                    SimpleY, CentroidSquares),
          linearity_error_squares(Count, SquaresX, SquaresY, SimpleX, SimpleY,
                                  Product, Slope, Intercept, ErrorSquares),
          Linearity is sqrt(ErrorSquares / CentroidSquares).


tolerance_distance(N, SquaresX, SquaresY, Tolerance) :-
          Tolerance is 0.0001 * sqrt((SquaresX + SquaresY) / N).


data_error_distance(N, SquaresX, SquaresY, SimpleX, SimpleY,
                    Product, Slope, Intercept, DataError) :-
          linearity_error_squares(N, SquaresX, SquaresY, SimpleX, SimpleY,
                                  Product, Slope, Intercept, ErrorSquares),
          DataError is sqrt(ErrorSquares / N).


centroid_distance_squares(N, SquaresX, SquaresY, SimpleX, SimpleY, CentroidSquares) :-
          CentroidSquares is (SquaresX- (SimpleX*SimpleX)/N)+(SquaresY- (SimpleY*SimpleY)/N).


linearity_error_squares(N, SquaresX, SquaresY, SimpleX, SimpleY,
                        Product, Slope, Intercept, ErrorSquares) :-
          SlopeSquare is Slope * Slope,
          ErrorSquares is ((1+SlopeSquare)/SlopeSquare) * (1/4)
                          * (SquaresY - 2*Slope*Product + SlopeSquare*SquaresX
                             - 2*Intercept*SimpleY + 2*Slope*Intercept*SimpleX
                             + Intercept*Intercept*N).

:- end_object.
