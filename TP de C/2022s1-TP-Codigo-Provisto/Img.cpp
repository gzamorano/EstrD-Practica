#include "Img.h"

#define DIR        int
#define HOJA       99
#define HORIZONTAL 42
#define VERTICAL   17
// constante para el render
#define UNITSIZE   50

#include "Matrix.h" 

struct ITreeSt {     
    DIR      division;    
    Color    color;       
    ITreeSt* first;    
    ITreeSt* second;
};
 /* INV.REP.
    // COMPLETAR
    OBS: si division es
      - HOJA, entonces color es el color del bloque representado  
      - HORIZONTAL, entonces first es la parte izquierda y second la derecha
      - VERTICAL, entonces first es la parte superior y second la inferior
 */

struct ImgSt {
    int width;
    int height;
    int size;
    ITreeSt* itree;
};
 /* INV.REP.
    // COMPLETAR
 */

//---------------------------------------------------------
// sizeImg
//---------------------------------------------------------
int sizeImg(Img img) {
    return img->size;
}


//---------------------------------------------------------
// createImg
//---------------------------------------------------------
// AUXILIAR SUGERIDA
ITreeSt* loadIT(int iw, int ih
               ,int fw, int fh
               ,int n, Matrix m, DIR d) {
    if(n == 1) {
      ITreeSt* itree = new ITreeSt;
      itree->division = HOJA;
      itree->color = M_getAt(m, iw, ih);
      itree->first = NULL;
      itree->second = NULL;
      return itree;
    } else if(n != 1 && d == HORIZONTAL) {
      ITreeSt* itree = new ITreeSt;
      itree->division = HORIZONTAL;
      itree->first = loadIT(iw, ih, fw, (fh/2), (n/2), m, VERTICAL);
      itree->second = loadIT(iw, (ih+fh/2), fw, (fh/2), (n/2), m, VERTICAL);
      return itree;
    } else if(n != 1 && d == VERTICAL) {
      ITreeSt* itree = new ITreeSt;
      itree->division = VERTICAL;
      itree->first = loadIT(iw, ih, (fw/2), (fh), (n/2), m, HORIZONTAL);
      itree->second = loadIT((iw+fw/2), ih, (fw/2), fh, (n/2), m, HORIZONTAL);
      return itree;
    }
}

// Esta funcion creo que ya no me sirve para nada
// int nextPow2(int n, int m) {
//     if(m>n){
//       return m;
//     } else {
//       return nextPow2(n, m*m);
//     }
// }


// PRECOND: w es potencia de 2, m es de w*w
Img createImg(Matrix m, int w) {
  int h = M_height(m);
  int s = w*h;
  ImgSt* img = new ImgSt;
  img->width = w;
  img->height = h;
  img->size = s;
  img->itree = loadIT(1,1,w,h,s,m,HORIZONTAL);

  return img;
}








//---------------------------------------------------------
// CompressImg
//---------------------------------------------------------


void buildH(int n1, int n2, ITreeSt* t1, ITreeSt t2) {
  if(t1->division == HOJA && t2->division == HOJA) {
    if(t1->color == t2->color){
      
    }
  } else {

  }


}

// cuando se encuentra con una hoja retorna 1 y sino siempre 0. ver eso despues como sumarlo.
// AUXILIAR SUGERIDA
// OBS: el int retornado es la cantidad final de hojas del t luego de modificarlo
int CompressIT(ITreeSt* t) {  
  // COMPLETAR
  if(t->division == HOJA) {
    return 1;
  } else if(t->division == HORIZONTAL) {
    int n1 = CompressIT(t->first);
    int n2 = CompressIT(t->second);
    
    buildH(n1, n2, t->first, t->second);
  
    // seria algo asi como return CompressIT(t->first) + CompressIt(t->second)
    // ver recorrido de arbol dfs bfs y como modificar el image tree.
    // ver tambien lo de 2 a la n.
    
  } else if(t->division == VERTICAL) {

  }
}


void CompressImg(Img img) {
  // COMPLETAR
  img->size = CompressIT(img->itree);  
}









int renderSize(int s) {
  return UNITSIZE * s;
}

void RenderBlock(int x, int y, int w, int h, Color color){
  // Renderiza cada "hoja o pixel de la imagen"
  cout << "<rect x=\"" << renderSize(x) << "\" y=\"" << renderSize(y) << "\" width=\"" << renderSize(w) << "\" height=\"" << renderSize(h) << "\" style=\"fill:";
  RenderColor(color,10);
  cout << ";stroke-width:3;stroke:rgb(0,0,0)\"/>" << endl;
}

//---------------------------------------------------------
// RenderImg
//---------------------------------------------------------
// AUXILIAR SUGERIDA
void RenderIT(int x, int y, int w, int h, ITreeSt* t) {
  // Renderiza el image tree
  if (t->division == HOJA) {
    RenderBlock(x, y, w, h, t->color);
  } else if (t->division == HORIZONTAL) {
    RenderIT(x, y,       w, (h/2), t->first);
    RenderIT(x, (y+h/2), w, (h/2), t->second);
  } else if (t->division == VERTICAL) {
    RenderIT(x,       y, (w/2), h, t->first);
    RenderIT((x+w/2), y, (w/2), h, t->second);
  } 
}

void RenderImg(Img img) {
  // Renderiza la imagen completa
  cout << "<svg height=\"" << renderSize(img->width) << "\" width=\"" << renderSize(img->height) << "\">" << endl; 
  RenderIT(0, 0, img->width, img->height, img->itree);
  cout << "</svg>" << endl;
}
 