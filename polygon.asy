/*

  polygon.asy

  Draw Fourier polygons from data in polygon.dat.
  
 */

import geometry;

bool is_near(point P, point Q) {
  return abs(P-Q) < 1.0e-12;
}

string label_list_to_string(int[] Vs, int shorten)
{
  // If shorten==-1 then don't shorten the labels.
  // Else any label longer than min(shorten, Vs.length)
  // is truncated with ellipsis \ldots.

  string result = "";
  
  if (shorten==-1) {
    for (int k=0; k<Vs.length; ++k) {
      if (k!=Vs.length-1)
	result = result + string(Vs[k]) + ", ";
      else 
	result = result + string(Vs[k]);
    }
    return result;
  } else {
    int max_label_length = min(shorten, Vs.length);
    for (int k=0; k<max_label_length; ++k) {
      if (k!=Vs.length-1) 
	result = result + string(Vs[k]) + ", ";
      else 
	result = result + string(Vs[k]); 
    }
    if (Vs.length>max_label_length)
      return result+"\ldots";
    else
      return result;
  }
}

struct PolyVertex {
  point V;
  int[] label_list = {};

  static PolyVertex PolyVertex(point Q, int my_label) {
    PolyVertex p = new PolyVertex;
    p.V = Q;
    p.label_list.push(my_label);
    return p;
  }
}
from PolyVertex unravel PolyVertex;

// the same PolyVertex struct can be used for
// reading in the original data and constructing
// the unique vertices with multi-labels.

file fin = input("polygon.dat");

int number_figures = fin;
int figure_numbering_base = 2;
bool show_vertex_labels = true;
int shorten_label_list = 4;

for (int nf=0; nf<number_figures; ++nf)
  {
    currentpicture = new picture;
    size(8cm);
    
    PolyVertex[] PVs;
    
    // Read data from file.
    int N = fin;
    for (int k=0; k<N; ++k) 
      PVs.push(PolyVertex((fin,fin), fin));
    
    
    // The 0th vertex is unique because that's where we start.
    
    PolyVertex[] unique_PVs = {PVs[0]};
    
    // Traverse through the array of PVs.
    // if PVs[k].V is in the uniqueVertex array,
    // then push its vertex label onto label_list.
    // if PVs[k].V is not in uniqueVertex array,
    // then push PVs[k] onto the end of unique_PVs list.
    
    for (int k=1; k<N; ++k) {
      bool found_flag = false;
      // find P[k] in the uniqueVertex array
      for (int j=0; j<unique_PVs.length; ++j) 
	if (is_near(unique_PVs[j].V, PVs[k].V)) {
	  unique_PVs[j].label_list.push(PVs[k].label_list[0]);
	  found_flag = true;
	  break;
	}
      if (!found_flag)
	unique_PVs.push(PVs[k]);
    }
    
    // Sketch axes and unit circle.
    
    real u = 1.25; // extents of axes lines.
    point O = (0,0);
    draw((-u,O.y)--(u,O.y), mediumgray);
    draw((O.x,-u)--(O.x,u), mediumgray);
    ellipse unitC = circle(O, 1.0);
    draw(unitC, pink+linewidth(1bp));
    
    // Print vertices and vertex lablel lists.

    // bool show_vertex_labels = true;
    // bool shorten_vertex_labels = false;
    // int vertex_label_limit = 4

    if (show_vertex_labels) {
      for (int k=0; k<unique_PVs.length; ++k) 
	dot("$" + label_list_to_string(unique_PVs[k].label_list,
				       shorten_label_list) + "$",
	    unique_PVs[k].V,  1.75*unit(unique_PVs[k].V-O));
    } else {
      for (int k=0; k<unique_PVs.length; ++k)
	dot(unique_PVs[k].V);
    }
    
    
    // Draw polygon edges.
    
    guide g;
    for (int k=0; k<unique_PVs.length; ++k) {
      g = g--(unique_PVs[k].V);
    }
    g = g--cycle;
    draw(g, blue+linewidth(1pt));
    
    // Debugging.
    for (int k=0; k<unique_PVs.length; ++k)
      write(string(unique_PVs[k].V.x) + ", " +
	    string(unique_PVs[k].V.y) + ", " 
	    + label_list_to_string(unique_PVs[k].label_list,
				   shorten_label_list));

    shipout("fig-"+string(figure_numbering_base)+"-"+string(nf));
  }
