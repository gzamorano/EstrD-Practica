#include "Img.h"

#define DIR        int
#define HOJA       99
#define HORIZONTAL 42
#define VERTICAL   17

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
    // COMPLETAR
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
    // COMPLETAR
    return img->size;
}


//---------------------------------------------------------
// createImg
//---------------------------------------------------------
// AUXILIAR SUGERIDA
ITreeSt* loadIT(int iw, int ih
               ,int fw, int fh
               ,int n, Matrix m, DIR d) {
    ITreeSt* itree = new ITreeSt;
    if(n == 1) {
      itree->division = HOJA;
      itree->color = M_getAt(m, iw, ih);
      itree->first = NULL;
      itree->second = NULL;
      return itree;
    } else if(n != 1 && d == HORIZONTAL) {
      itree->division = HORIZONTAL;
      itree->first = loadIT(iw, ih, fw, (fh/2), (n/2), m, VERTICAL);
      itree->second = loadIT(iw, (ih+fh/2), fw, (fh/2), (n/2), m, VERTICAL);
      return itree;
    } else if(n != 1 && d == VERTICAL) {
      itree->division = VERTICAL;
      itree->first = loadIT(iw, ih, (fw/2), (fh), (n/2), m, HORIZONTAL);
      itree->second = loadIT((iw+fw/2), ih, (fw/2), fh, (n/2), m, HORIZONTAL);
      return itree;
    }
  
}

int nextPow2(int n) {
    
    if() { 
      return 
    }
}

// PRECOND: w es potencia de 2, m es de w*w
Img createImg(Matrix m, int w) {
  // COMPLETAR
  int ch = M_width(m);
  int cw = M_height(m);
  int h = nextPow2(ch);
  int w = nextPow2(cw);
  int n = max(w, h);
  


  // aca tengo que devolver una img 
  return 
}

//---------------------------------------------------------
// CompressImg
//---------------------------------------------------------
// AUXILIAR SUGERIDA
// OBS: el int retornado es la cantidad final de hojas del t luego de modificarlo
int CompressIT(ITreeSt* t) {  
}

void CompressImg(Img img) {
  // COMPLETAR
}

//---------------------------------------------------------
// RenderImg
//---------------------------------------------------------
// AUXILIAR SUGERIDA
void RenderIT(int x, int y, int w, int h, ITreeSt* t) {
  
}

void RenderImg(Img img) {
  // COMPLETAR
}
