# About

These examples are Delphi VCL projects that show the use of some components of the mrMath library. 
The paltform that was used to compile is Delphi 2010 so transition to higher Delphi versions should work out of the box.

Please don't judge about the look ;) these are some simple test programs.

# TSNE

The sample creates a set of around 100 datapoints either arranged in a circle, two gaussian distributed point clouds or 
a very simple line arrangement.

You can steer the number of iterations and the perplexity and apply the use of the parameter _theta_ - this parameter should be between 0 and 1.
A value of 0 applies the exact version of TSNE. A value <> 0 let the class switch to the _Barnes Hut_ algorithm and it defines how much the points 
are grouped together in the tree nodes.

# Spline Test

This example shows how the *BSplines* in action. A variety of parameters can be set to test the robustness against outliers and the effect of
least squares fitting. You may define the order and the number of _breaks_ - the number of base points in the algorithm.

# Regression test

Actually fitting a polynom to a point cloud - it supports linear, quadratic and cubic regression as well as a non linear fitting algorithm that
fits a _arcTan_ to a point cloud using the _Levenberg Marquard_ algorithm.

# EM Test

The _Expectation Maximization_ algorithm is applied to a 2D point cloud. You can choose the number of centers (up to 4), their variance, and even show the
iterative animation of the algorithm.