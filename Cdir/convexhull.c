#define qh_QHimport
#include "qhull_ra.h"
#include "convexhull.h"
#include "utils.h"

// to use the qsort function - sort vertices according to their ids
int cmpvertices (const void * a, const void * b) {
   return ( (*((VertexT*)a)).id - (*((VertexT*)b)).id );
}
// - sort full vertices
int cmpfullvertices (const void * a, const void * b) {
  return ( (*((FullVertexT*)a)).id - (*((FullVertexT*)b)).id );
}
// - sort edges
int cmpedges (const void * a, const void * b) {
  if((*(unsigned**)a)[0] > (*(unsigned**)b)[0]){
    return 1;
  }else if((*(unsigned**)a)[0] == (*(unsigned**)b)[0]){
    return (*(unsigned**)a)[1] - (*(unsigned**)b)[1];
  }else{
    return -1;
  }
}

/* test equality of two _sorted_ arrays */
unsigned equalarraysu(unsigned* array1, unsigned* array2, unsigned length){
  unsigned i;
  for(i=0; i < length; i++){
    if(array1[i] != array2[i]){
      break;
    }
  }
  return i == length;
}

/* return ids of a vector of VertexT */
unsigned* map_vertexid(VertexT* vertices, unsigned nvertices){
  unsigned* ids = malloc(nvertices * sizeof(unsigned));
  for(unsigned v=0; v < nvertices; v++){
    ids[v] = vertices[v].id;
  }
  return ids;
}

/* return ids of a vector of RidgeT */
unsigned* map_ridgeid(RidgeT* ridges, unsigned nridges){
  unsigned* ids = malloc(nridges * sizeof(unsigned));
  for(unsigned r=0; r < nridges; r++){
    ids[r] = ridges[r].id;
  }
  return ids;
}

// void deepCopyRidge(RidgeT* src, RidgeT* dest) { et dim !
//     dest = malloc(sizeof(RidgeT));
//     *dest = *src;
//     dest->vertices = malloc(src->nvertices * sizeof(VertexT));
//     for(unsigned v=0; v<src->nvertices; v++){
//       dest->vertices[v].id = src.vertices[v].id;
//       memcpy(dest->vertices[v].point, dest->vertices[v].point, dim * sizeof(double));
//     }
// }

/* deep copy of a ridge */
RidgeT copyRidge(RidgeT ridge, unsigned dim){
  RidgeT out;
  out.ridgeOf1  = ridge.ridgeOf1;
  out.ridgeOf2  = ridge.ridgeOf2;
  out.nvertices = ridge.nvertices;
  out.nedges    = ridge.nedges;
  out.vertices  = malloc(out.nvertices * sizeof(VertexT));
  for(unsigned v=0; v < out.nvertices; v++){
    out.vertices[v].id    = ridge.vertices[v].id;
    out.vertices[v].point = malloc(dim * sizeof(double));
    for(unsigned i=0; i < dim; i++){
      out.vertices[v].point[i] = ridge.vertices[v].point[i];
    }
  }
  // out.edges = malloc(out.nedges * sizeof(unsigned*));
  // for(unsigned e=0; e < out.nedges; e++){
  //   out.edges[e] = malloc(2 * sizeof(unsigned));
  //   out.edges[e][0] = ridge.edges[e][0];
  //   out.edges[e][1] = ridge.edges[e][1];
  // }
  return out;
}

/* append to a vector of VertexT */
void appendv(VertexT x, VertexT** array, unsigned length, unsigned* flag){
  *flag = 1;
  for(unsigned i=0; i < length; i++){
    if(x.id == (*(*array + i)).id){
      *flag = 0;
      break;
    }
  }
  if(*flag == 1){
    *array = realloc(*array, (length+1) * sizeof(VertexT));
    if(*array == NULL){
      printf("realloc failure - exiting\n");
      exit(1);
    }
    *(*array + length) = x;
  }
}

/* union of two vectors of VertexT */
void unionv(VertexT** vs1, VertexT* vs2, unsigned l1, unsigned l2, unsigned* l){
  *l = l1;
  for(unsigned v=0; v < l2; v++){
    unsigned pushed;
    appendv(vs2[v], vs1, *l, &pushed);
    if(pushed){
      (*l)++;
    }
  }
  /* sort vertices according to their ids */
  qsort(*vs1, *l, sizeof(VertexT), cmpvertices);
}

/* merge ridges with same ridgeOf's */
RidgeT* mergeRidges(RidgeT* ridges, unsigned nridges, unsigned* newlength){
  // http://www.c4learn.com/c-programs/to-delete-duplicate-elements-in-array.html
  *newlength = nridges;
  unsigned i,j,k;
  for(i = 0; i < nridges; i++){
    for(j = i+1; j < nridges; ){
      if(ridges[i].ridgeOf1 == ridges[j].ridgeOf1 &&
         ridges[i].ridgeOf2 == ridges[j].ridgeOf2)
      {
        unsigned l;
        unionv(&(ridges[i].vertices), ridges[j].vertices,
                 ridges[i].nvertices, ridges[j].nvertices, &l);
        ridges[i].nvertices = l;
        (*newlength)--;
        for(k = j; k+1 < nridges; k++){
          ridges[k] = ridges[k+1];
        }
        nridges--;
      }else{
        j++;
      }
    }
  }
  RidgeT* out = malloc(*newlength * sizeof(RidgeT));
  for(unsigned r=0; r < *newlength; r++){
    out[r] = ridges[r];
  }
  return out;
}

/* all ridges from the ridges stored in the faces */
RidgeT* allRidges(FaceT *faces, unsigned nfaces, unsigned dim, unsigned* length){
  RidgeT* out = malloc(faces[0].nridges * sizeof(RidgeT));
  for(unsigned i=0; i < faces[0].nridges; i++){
    out[i] = copyRidge(faces[0].ridges[i], dim);
    out[i].id = i;
    out[i].nedges = 0;
    // RidgeT out[i];
    // deepCopyRidge(&(faces[0].ridges[i]), &(out[i]));
  }
  *length    = faces[0].nridges;
  unsigned n = faces[0].nridges;
  for(unsigned f=1; f < nfaces; f++){
    for(unsigned j=0; j < faces[f].nridges; j++){
      unsigned count = 0;
      for(unsigned i=0; i < n; i++){
        unsigned flag = 0;
        for(unsigned v=0; v < faces[f].ridges[j].nvertices; v++){
          if(faces[f].ridges[j].vertices[v].id != out[i].vertices[v].id){
            flag = 1;
            break;
          }
        }
        if(flag){
          count++;
        }else{
          break;
        }
      }
      if(count == n){
        out          = realloc(out, (*length+1) * sizeof(RidgeT));
        if(out == NULL){
          printf("realloc failure - exiting\n");
          exit(1);
        }
        out[*length] = copyRidge(faces[f].ridges[j], dim);
        out[*length].id = *length;
        out[*length].nedges = 0;
        // RidgeT out[*length];
        // deepCopyRidge(&(faces[f].ridges[j]), &(out[*length]));
        (*length)++;
      }
    }
    n = *length;
  }
  return out;
}

/* assign ids to the ridges stored in the faces */
void assignRidgesIds(FaceT** faces, unsigned nfaces, RidgeT* allridges,
                     unsigned nallridges)
{
  for(unsigned f=0; f < nfaces; f++){
    for(unsigned fr=0; fr < (*(*faces + f)).nridges; fr++){
      for(unsigned r=0; r < nallridges; r++){
        if((allridges[r].nvertices == (*(*faces + f)).ridges[fr].nvertices) &&
            equalarraysu(map_vertexid(allridges[r].vertices,
                                      allridges[r].nvertices),
                         map_vertexid((*(*faces + f)).ridges[fr].vertices,
                                      allridges[r].nvertices),
                         allridges[r].nvertices))
        {
          (*(*faces + f)).ridges[fr].id = allridges[r].id;
          break;
        }
      }
    }
  }
}

// double* ridgeCentroid(RidgeT ridge, unsigned dim){
//   double* out = malloc(dim * sizeof(double));
//   for(unsigned i=0; i<dim; i++){
//     out[i] = 0;
//     for(unsigned v=0; v<dim-1; v++){
//       out[i] += ridge.vertices[v].point[i];
//     }
//     out[i] /= dim - 1;
//   }
//   return out;
// }

/* the threshold distance to detect neighbor vertices */
double ridgeMaxDistance(RidgeT ridge, unsigned v, unsigned dim){
  double dists[ridge.nvertices-1];
  unsigned count = 0;
  for(unsigned w=0; w < ridge.nvertices; w++){
    if(w != v){
      dists[count] = squaredDistance(ridge.vertices[v].point,
                                     ridge.vertices[w].point, dim);
      count++;
    }
  }
  qsort(dists, ridge.nvertices-1, sizeof(double), cmpfuncdbl);
  return dists[1];
}

/* neighbor vertices of a vertex from all ridges, for dim>2 */
unsigned* neighVertices(unsigned id, RidgeT* allridges, unsigned nridges,
                        unsigned dim, unsigned triangulate, unsigned* lengthout)
{
  unsigned* neighs = malloc(0);
  *lengthout = 0;
  for(unsigned e=0; e < nridges; e++){
    for(unsigned v=0; v < allridges[e].nvertices; v++){
      if(id == allridges[e].vertices[v].id){
        for(unsigned w=0; w < allridges[e].nvertices; w++){
          if(w != v && (triangulate || dim == 3 || // dim3 pas besoin de tester la distance: il n'y a que deux vertices connectés
             squaredDistance(allridges[e].vertices[w].point,
                             allridges[e].vertices[v].point, dim) <=
              ridgeMaxDistance(allridges[e], v, dim)))
          {
            unsigned pushed;
            appendu(allridges[e].vertices[w].id, &neighs, *lengthout, &pushed);
            if(pushed){
              (*lengthout)++;
            }
          }
        }
        break;
      }
    }
  }
  return neighs;
}

/* neighbor ridges of a vertex */
unsigned* neighRidges(unsigned id, RidgeT* allridges, unsigned nridges,
                     unsigned* length)
{
  unsigned* neighs = malloc(0);
  *length = 0;
  for(unsigned e=0; e < nridges; e++){
    unsigned flag = 0;
    for(unsigned v=0; v < allridges[e].nvertices; v++){
      if(id == allridges[e].vertices[v].id){
        flag = 1;
        break;
      }
    }
    if(flag){
      neighs = realloc(neighs, (*length+1)*sizeof(unsigned));
      if(neighs == NULL){
        printf("realloc failure - exiting\n");
        exit(1);
      }
      neighs[*length] = e;
      (*length)++;
    }
  }
  return neighs;
}

/* whether distinct x1 and x2 belong to array of distinct values */
unsigned areElementsOf(unsigned x1, unsigned x2, unsigned* array,
                       unsigned length)
{
  unsigned count = 0;
  for(unsigned i=0; (i < length) && (count < 2); i++){
    if(x1 == array[i] || x2 == array[i]){
      count++;
    }
  }
  return count==2;
}

/* make face/ridge edges from all edges */
unsigned** makeEdges(SetOfVerticesT face, unsigned** alledges,
                     unsigned nalledges, unsigned* lengthout)
{
  *lengthout = 0;
  unsigned* faceverticesids = map_vertexid(face.vertices, face.nvertices);
  unsigned flags[nalledges];
  for(unsigned e=0; e < nalledges; e++){
    if(areElementsOf(alledges[e][0], alledges[e][1], faceverticesids,
                     face.nvertices))
    {
      flags[e] = 1;
      (*lengthout)++;
    }else{
      flags[e] = 0;
    }
  }
  unsigned** out = malloc(*lengthout * sizeof(unsigned*));
  unsigned count = 0;
  for(unsigned e=0; e < nalledges; e++){
    if(flags[e] == 1){
      out[count] = alledges[e];
      count++;
    }
  }
  return out;
}

/* all edges from all vertices */
unsigned** allEdges(FullVertexT* vertices, unsigned nvertices,
                    unsigned outlength)
{
  unsigned** out = malloc(outlength * sizeof(unsigned*));
  for(unsigned i=0; i < vertices[0].nneighsvertices; i++){
    out[i] = malloc(2 * sizeof(unsigned));
    out[i][0] = vertices[0].id;
    out[i][1] = vertices[0].neighvertices[i];
    qsortu(out[i], 2);
  }
  unsigned n = vertices[0].nneighsvertices;
  for(unsigned v=1; v < nvertices; v++){
    unsigned ids[2];
    for(unsigned i=0; i < vertices[v].nneighsvertices; i++){
      ids[0] = vertices[v].id;
      ids[1] = vertices[v].neighvertices[i];
      qsortu(ids, 2);
      unsigned j;
      for(j=0; j < n; j++){
        if(ids[0] == out[j][0] && ids[1] == out[j][1]){
          break;
        }
      }
      if(j == n){
        out[n] = malloc(2 * sizeof(unsigned));
        out[n][0] = ids[0]; out[n][1] = ids[1];
        n++;
      }
      if(n == outlength){
        break;
      }
    }
    if(n == outlength){
      break;
    }
  }
  return out;
}

// ATTENTION avec Qt le center dans facet->center est le centre de l'union des triangles,
//  (ainsi que normal et offset mais ça ok)

// un ridge est simplicial ; pour l'hypercube il y a 2 ridges entre 2 faces,
// ils forment le carré à l'intersection

/* main function */
ConvexHullT* convexHull(
	double*   points,
	unsigned  dim,
	unsigned  n,
  unsigned  triangulate,
  unsigned  print,
  char*     summaryFile,
	unsigned* exitcode
)
{
	char opts[250]; /* option flags for qhull, see qh_opt.htm */
  sprintf(opts, "qhull s FF %s", triangulate ? "Qt" : "");
	qhT qh_qh;       /* Qhull's data structure */
  qhT *qh= &qh_qh;
  QHULL_LIB_CHECK
  qh_meminit(qh, stderr);
	boolT ismalloc  = False; /* True if qhull should free points in qh_freeqhull() or reallocation */
	FILE *errfile   = NULL;
  FILE* outfile;
  if(print){
    outfile = stdout;
  }else{
    outfile = NULL;
  }
  qh_zero(qh, errfile);
	exitcode[0] = qh_new_qhull(qh, dim, n, points, ismalloc, opts, outfile,
                             errfile);
  //fclose(tmpstdout);
  printf("exitcode: %u\n", exitcode[0]);

  ConvexHullT* out = malloc(sizeof(ConvexHullT));

	if (!exitcode[0]) {  /* 0 if no error from qhull */

    /* print summary to file */
    if(*summaryFile != 0){
      FILE* sfile = fopen(summaryFile, "w");
    	qh_printsummary(qh, sfile);
    	fclose(sfile);
    }

    //qh_getarea(qh, qh->facet_list); // no triowner if I do that; do qh_facetarea, not facet->f.area

    unsigned   nfaces    = qh->num_facets;
    FaceT*     faces     = malloc(nfaces * sizeof(FaceT));
    {
      facetT *facet; unsigned i_facet = 0;
      FORALLfacets{
        facet->id                  = i_facet; /* for neighbors and ridgeOf */
        faces[i_facet].area        = qh_facetarea(qh, facet);
        double* center             = qh_getcenter(qh, facet->vertices);
        faces[i_facet].center      = malloc(dim * sizeof(double));
        for(unsigned i=0; i < dim; i++){
          faces[i_facet].center[i] = center[i];
        }
        double* normal = facet->normal;
        faces[i_facet].normal      = malloc(dim * sizeof(double));
        for(unsigned i=0; i < dim; i++){
          faces[i_facet].normal[i] = normal[i];
        }
        faces[i_facet].offset      = facet->offset;
        faces[i_facet].nvertices   = (unsigned) qh_setsize(qh, facet->vertices);
        { /* face vertices */
          faces[i_facet].vertices =
            (VertexT*) malloc(faces[i_facet].nvertices * sizeof(VertexT));
          vertexT *vertex, **vertexp;
          unsigned i_vertex = 0;
          FOREACHvertex_(facet->vertices){
            faces[i_facet].vertices[i_vertex].id =
              (unsigned) qh_pointid(qh, vertex->point);
            faces[i_facet].vertices[i_vertex].point =
              malloc(dim * sizeof(double));
            faces[i_facet].vertices[i_vertex].point =
              getpoint(points, dim, faces[i_facet].vertices[i_vertex].id);
            // plante dans Haskell: (faces[i_facet].vertices)[i_vertex].point = vertex->point;
            i_vertex++;
          }
          qsort(faces[i_facet].vertices, faces[i_facet].nvertices,
                sizeof(VertexT), cmpvertices);
        }
        if(dim == 3){ /* orientation of the normals */
          pointT* onepoint = ((vertexT*)facet->vertices->e[0].p)->point;
          double thepoint[dim]; /* onepoint+normal */
          for(unsigned i=0; i < dim; i++){
            thepoint[i] = onepoint[i] + faces[i_facet].normal[i];
          }
          /* we check that these two points are on the same side of the ridge */
          double h1 = dotproduct(qh->interior_point,
                                 faces[i_facet].normal, dim) +
                      faces[i_facet].offset;
          double h2 = dotproduct(thepoint, faces[i_facet].normal, dim) +
                      faces[i_facet].offset;
          if(h1*h2 > 0){
            for(unsigned i=0; i < dim; i++){
              faces[i_facet].normal[i] *= -1;
            }
            printf("change sign\n"); // seems to never occur
          }else{
            printf("not change sign\n");
          }
        }
        /**/
        i_facet++;
      }
    }

    { /* neighbor faces, faces families, and ridges */
      facetT *facet;
      unsigned i_facet = 0;
      FORALLfacets{
        {
          faces[i_facet].neighborsize = qh_setsize(qh, facet->neighbors);
          faces[i_facet].neighbors =
            malloc(faces[i_facet].neighborsize * sizeof(unsigned));
          unsigned i_neighbor = 0;
          facetT *neighbor, **neighborp;
          FOREACHneighbor_(facet){
            faces[i_facet].neighbors[i_neighbor] = (unsigned) neighbor->id;
            i_neighbor++;
          }
          qsortu(faces[i_facet].neighbors, faces[i_facet].neighborsize);
        }
        { /* face family, when option Qt */
          if(facet->tricoplanar){
            faces[i_facet].family = facet->f.triowner->id;
          }else{
            faces[i_facet].family = -1;
          }
        }
        { /* face ridges */
          qh_makeridges(qh, facet);
          unsigned nridges = qh_setsize(qh, facet->ridges);
          RidgeT* ridges = malloc(nridges * sizeof(RidgeT));
          ridgeT *ridge, **ridgep;
          unsigned i_ridge = 0;
          FOREACHridge_(facet->ridges){
            ridges[i_ridge].nedges = 0;
            unsigned ridgeSize = qh_setsize(qh, ridge->vertices); // dim-1
//            printf("ridge size: %u\n", ridgeSize);
            ridges[i_ridge].nvertices = ridgeSize;
            unsigned ids[ridgeSize];
            for(unsigned v=0; v < ridgeSize; v++){
              ids[v] =
                qh_pointid(qh, ((vertexT*)ridge->vertices->e[v].p)->point);
            }
            qsortu(ids, ridgeSize);
            ridges[i_ridge].vertices = malloc(ridgeSize * sizeof(VertexT));
            for(unsigned v=0; v < ridgeSize; v++){
              ridges[i_ridge].vertices[v].id = ids[v];
              ridges[i_ridge].vertices[v].point = getpoint(points, dim, ids[v]);
            }
            unsigned ridgeofs[2];
            ridgeofs[0] = ridge->bottom->id;
            ridgeofs[1] = ridge->top->id;
            qsortu(ridgeofs, 2);
            ridges[i_ridge].ridgeOf1 = ridgeofs[0];
            ridges[i_ridge].ridgeOf2 = ridgeofs[1];
            /**/
            i_ridge++;
          }
          /* merge triangulated ridges */
          if(dim > 3 && !triangulate){
            unsigned l;
            faces[i_facet].ridges  = mergeRidges(ridges, nridges, &l);
            faces[i_facet].nridges = l;
          }else{ /* dim 2 or 3, or triangulate option */
            faces[i_facet].ridges  = ridges;
            faces[i_facet].nridges = nridges;
          }
        }
        /**/
        i_facet++;
      }
    }

    /* make unique ridges */
    unsigned n_allridges;
    RidgeT* allridges = allRidges(faces, nfaces, dim, &n_allridges);
//    printf("nallridges: %u\n", n_allridges);

    /* assign ridges ids to the ridges stored in the faces */
    assignRidgesIds(&faces, nfaces, allridges, n_allridges);

    /* all vertices */
    unsigned nvertices = qh->num_vertices;
    FullVertexT* vertices = malloc(nvertices * sizeof(FullVertexT));
    {
      qh_vertexneighbors(qh); /* make the neighbor facets of the vertices */
      vertexT *vertex;
      unsigned i_vertex=0;
      FORALLvertices{
        /* vertex id and coordinates */
        vertices[i_vertex].id    = (unsigned) qh_pointid(qh, vertex->point);
        vertices[i_vertex].point = getpoint(points, dim, vertices[i_vertex].id);

        /* neighbor facets of the vertex */
        vertices[i_vertex].nneighfacets = qh_setsize(qh, vertex->neighbors);
        vertices[i_vertex].neighfacets =
          malloc(vertices[i_vertex].nneighfacets * sizeof(unsigned));
        facetT *neighbor, **neighborp;
        unsigned i_neighbor = 0;
        FOREACHneighbor_(vertex){
          vertices[i_vertex].neighfacets[i_neighbor] = neighbor->id;
          i_neighbor++;
        }
        qsortu(vertices[i_vertex].neighfacets, vertices[i_vertex].nneighfacets);

        /* neighbor vertices of the vertex */
        if(dim > 2){
          unsigned nneighsvertices;
          vertices[i_vertex].neighvertices =
            neighVertices(vertices[i_vertex].id, allridges, n_allridges,
                          dim, triangulate, &nneighsvertices);
          vertices[i_vertex].nneighsvertices = nneighsvertices;
        }else{ /* dim=2 */
          vertices[i_vertex].nneighsvertices = 2;
          vertices[i_vertex].neighvertices   = malloc(2 * sizeof(unsigned));
          unsigned count = 0;
          for(unsigned f=0; f < nfaces; f++){
            for(unsigned i=0; i < 2; i++){
              if(faces[f].vertices[i].id == vertices[i_vertex].id){
                vertices[i_vertex].neighvertices[count] =
                  faces[f].vertices[1-i].id;
                count++;
                break;
              }
            }
            if(count == 2){
              break;
            }
          }
        }
        qsortu(vertices[i_vertex].neighvertices,
               vertices[i_vertex].nneighsvertices);

        /* neighbor ridges of the vertex */
        if(dim > 2){
          unsigned nneighridges;
          vertices[i_vertex].neighridges =
            neighRidges(vertices[i_vertex].id, allridges, n_allridges,
                       &nneighridges);
          qsortu(vertices[i_vertex].neighridges, nneighridges);
          vertices[i_vertex].nneighridges = nneighridges;
        }else{ /* dim=2 */
          vertices[i_vertex].nneighridges = 0; /* ridge = vertex singleton */
        }
        /**/
        i_vertex++;
      }
      /* sort vertices according to their ids */
      qsort(vertices, nvertices, sizeof(FullVertexT), cmpfullvertices);
    }

    /* all edges */
    unsigned nalledges = 0;
    for(unsigned v=0; v < nvertices; v++){
      nalledges += vertices[v].nneighsvertices;
    }
    nalledges /= 2;
    unsigned** alledges = allEdges(vertices, nvertices, nalledges);
    qsort(alledges, nalledges, sizeof(unsigned*), cmpedges);

    { /* faces edges and ridges ids */
      facetT *facet; unsigned i_facet=0;
      FORALLfacets{
        /* facet ridges ids */
        faces[i_facet].ridgesids =
          map_ridgeid(faces[i_facet].ridges, faces[i_facet].nridges);
        qsortu(faces[i_facet].ridgesids, faces[i_facet].nridges);
        /* facet edges */
        SetOfVerticesT facet_vset = {.vertices = faces[i_facet].vertices,
                                     .nvertices = faces[i_facet].nvertices};
        unsigned nfaceedges;
        faces[i_facet].edges =
          makeEdges(facet_vset, alledges, nalledges, &nfaceedges);
        //qsort(faces[i_facet].edges, nfaceedges, sizeof(unsigned*), cmpedges); useless, I think
        faces[i_facet].nedges = nfaceedges;
        /**/
        i_facet++;
      }
    }

    /* ridges edges */
    if(dim > 3){
      for(unsigned r=0; r < n_allridges; r++){
        unsigned facetid = allridges[r].ridgeOf1;
        SetOfVerticesT vset = {.vertices = allridges[r].vertices,
                               .nvertices = allridges[r].nvertices};
        unsigned nedges;
        allridges[r].edges =
          makeEdges(vset, faces[facetid].edges, faces[facetid].nedges, &nedges);
        allridges[r].nedges = nedges;
      }
    }

    /* output */
    out->dim       = dim;
    out->vertices  = vertices;
    out->nvertices = nvertices;
    out->faces     = faces;
    out->nfaces    = nfaces;
    out->ridges    = allridges;
    out->nridges   = n_allridges;
    out->edges     = alledges;
    out->nedges    = nalledges;

  } // end if exitcode

  /* Do cleanup regardless of whether there is an error */
  int curlong, totlong;
	qh_freeqhull(qh, !qh_ALL);               /* free long memory */
	qh_memfreeshort(qh, &curlong, &totlong); /* free short memory and memory allocator */

  printf("RETURN\n");
  if(*exitcode){
    free(out);
    return 0;
  }else{
    return out;
  }

}
