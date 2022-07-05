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

int nextPow2(int n, int m) {
    if(m>n){
      return m;
    } else {
      return nextPow2(n, m*m);
    }
}


// PRECOND: w es potencia de 2, m es de w*w
Img createImg(Matrix m, int w) {
  //int w = M_width(m);
  int h = M_height(m);
  // int h = nextPow2(ch, 2);
  //     w = nextPow2(cw, 2);
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
// AUXILIAR SUGERIDA
// OBS: el int retornado es la cantidad final de hojas del t luego de modificarlo
// int CompressIT(ITreeSt* t) {  
// }

// void CompressImg(Img img) {
//   // COMPLETAR
// }

int renderSize(int s) {
  return UNITSIZE * s;
}


void RenderBlock(int x, int y, int w, int h, Color color){
  // "esto seria apendeado" >> ss.svg
  // f es FINAL fh fw, ver eso
  cout << "<rect x=\"" << renderSize(x) << "\" y=\"" << renderSize(y) << "\" width=\"" << renderSize(w) << "\" height=\"" << renderSize(h) << "\" style=\"fill:";
  RenderColor(color,10);
  cout << ";stroke-width:3;stroke:rgb(0,0,0)\"/>" << endl;
}

//---------------------------------------------------------
// RenderImg
//---------------------------------------------------------
// AUXILIAR SUGERIDA
void RenderIT(int x, int y, int w, int h, ITreeSt* t) {
  // COMPLETAR
  if (t->division == HOJA) {
    RenderBlock(x, y, w, h, t->color);
  } else if (t->division == HORIZONTAL) {
    RenderIT(x, y,       w, (h/2), t->first);
    RenderIT(x, (y+h/2), w, (h/2), t->second);
  } else if (t->division == VERTICAL) {
    RenderIT(x,       y, (w/2), h, t->first);
    RenderIT((x+w/2), y, w,     h, t->second);
  }
  
}

void RenderImg(Img img) {
  // COMPLETAR
  // Dentro de esta función se debería invocar a RenderIT pasandole 0 0 w h t
  cout << "<svg height=\"" << renderSize(img->width) << "\" width=\"" << renderSize(img->height) << "\">" << endl; 
  RenderIT(0, 0, img->width, img->height, img->itree);
  cout << endl;
  cout << "</svg>";
}
 