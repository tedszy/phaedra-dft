import geometry;
size(8cm);

file fin = input("polygon.dat");

real[] x;
real[] y;
int[] v;

// Read in N, the number of vertices.

int N = fin;

for (int k=0; k<N; ++k) {
  x[k] = fin;
  y[k] = fin;
  v[k] = fin;
}

x.cyclic=true;
y.cyclic=true;

point O = (0,0);

real u = 1.25;
draw((-u,0)--(u,0), gray);
draw((0,-u)--(0,u), gray);

ellipse unitC = circle(O, 1.0);
draw(unitC, lightred+linewidth(1bp));



for (int k=0; k<N; ++k) {
  draw((x[k],y[k])--(x[k+1],y[k+1]), blue+linewidth(1pt));
}

for (int k=0; k<N; ++k) {
  dot(format("$%d$", v[k]), (x[k],y[k]), unit((x[k],y[k])-O));
}

