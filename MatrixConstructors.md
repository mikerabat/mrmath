# Matrix class #

## Construction of objects ##

To construct a matrix obj there are 4 different constructors available:

```
constructor Create; 
```

Creates an 1x1 matrix with width and height is 1 the value is initialized to zero.

```
constructor Create(width, height : integer; const initVal : double = 0);
```

Creates a new matrix with the given width and height and initializes each matrix element with
the given value. Note that 0 width or zero height is not allowed.

```
constructor Create(data : PDouble; LineWidth : integer; width, height : integer);
```

This constructor initializes the internal datastructures with the given parameters.
  * **data**: points to the first element of the matrix.
  * **LineWidth**: Number of bytes between consecutive rows.
  * **width, height**: Matrix size

Note that this constructor takes ownership of the memory given with **data** and will release it when the object is destroyed! Use one of the **Assign** methods to move the data instead.

```
constructor Create(const Data : TDoubleDynArray; width, height : integer); overload;
```

Reserves memory such that the matrix width and height can be accessed and then copies the content of **data** to the resulting matrix. **Data** needs to be exactly as long as _width\*height_ and is interpreted row wise.

### Examples ###

```

// create an empty matrix
procedure foo;
var mtx : IMatrix;
begin
     mtx := TDoubleMatrix.Create;

     assert(mtx.width = 1, 'Error');
end;

// create a 3x3 matrix and initialize the matrix with 3
procedure foo;
var mtx : IMatrix;
begin
     mtx := TDoubleMatrix.Create(3, 3, 3);

     assert(mtx.width = 3, 'Error');
     assert(mtx[2, 2] = 3, 'Error');
end;

// create a 3x3 matrix and take ownership of the data
procedure foo;
var mtx : IMatrix;
    data : PDouble; 
begin
     data := GetMemory(4*3*sizeof(double));  // create a 16 byte row wise aligned matrix 
     FillChar(data^, 0, 4*3*sizeof(double)); // fill with zeros
     mtx := TDoubleMatrix.Create(data, 4*sizeof(double), 3, 3);

     mtx[0, 0] := 1;
     assert(data^ = 1, 'Error');
     assert(mtx.width = 3, 'Error');
end;


// create a 3x3 matrix and initialize with a dynamic array
procedure foo;
var mtx : IMatrix;
    data : TDoubleDynArray; 
begin
     SetLength(data, 9); //3x3 matrix

     for i := 0 to Length(data) - 1 do
         data[i] := i;
     
     mtx := TDoubleMatrix.Create(data, 3, 3);

     assert(mtx.width = 3, 'Error');
     assert(mtx[2, 2] = 8, 'Error');
end;

```

## Memory Management ##

The matrix class basically handles the memory management and allocating aligned memory on it's own. To optimize the memory management most of the simple matrix functions are built in two favors - one in memory and the other one returns the destination matrix e.g.: addition of two matrices can be executed either in place or using a third destination matrix:

```
procedure add;
var mt1, mt2 : IMatrix;
begin
     // creation and assigning...

     // addition
     mt1 := mt1.Add(mt2);

     // or
     mt1.AddInPlace(mt2);
end;
```

both methods here are perfectly valid. The difference is the memory allocation scheme. In the first function a third matrix is allocated and the result is then stored there. In the second - in place - function the second allocation is not necessary.

Note that not all in place functions can preserver that optimal memory management e.g. matrix multiplication cannot be executed in place due to the dependencies. There internally a temporary matrix is allocated which overwrites the original one.

To optimize memory management one can also only select a certain sub block of a matrix using

```
procedure SetSubMatrix(x, y, Subwidth, Subheight : integer);
```

and revert that selection by:

```
procedure UseFullMatrix;
```

Note that the _SetSubMatrix_ function is especially handy for right hand side operations and it`'`s params are always in terms of the full matrix meaning that subsequent selections by this function are always selections on the full matrix meaning that _UseFullMatrix_ is implicitly called by each _SetSubMatrix_ call e.g.:

```
procedure Select;
var mt : IMatrix;
begin
     mt := TDoubleMatrix.Create(10, 10);
     // selects 3x3 matrix starting at row 3 column 3
     mt.SetSubMatrix(3,3, 3, 3); 


     // select 2x2 matrix starting at row 5 column 5
     mt.SetSubMatrix(5,5, 2,2);
end;
```

In place function not affecting the current state of the matrix and are only applied to the current submatrix without changing the rest:
  * AddInPlace
  * SubInPlace
  * ElementWiseMultInPlace
  * AddAndScaleInPlace
  * ScaleAndAddInPlace
  * ScaleInPlace
  * SQRTInPlace
  * ElementwiseFuncInPlace

## Assigning data ##

There are basically three methods avail to assign data to a matrix:

  * Within the constructor
  * Using on of the assign operators
  * Assign data column or row wise

The constructor allows to initialize a matrix with the given data. This data is not owned by the matrix but rather copied.

One can use one of the following assign operators:

```
procedure Assign(Value : TDoubleMatrix); 
procedure Assign(Value : IMatrix); 
procedure Assign(Value : TDoubleMatrix; OnlySubElements : boolean);     procedure Assign(Value : IMatrix; OnlySubElements : boolean); 
procedure Assign(const Mtx : Array of double; W, H : integer);    procedure AssignSubMatrix(Value : TDoubleMatrix; X : integer = 0; Y : integer = 0); 
procedure AssignSubMatrix(Value : IMatrix; X : integer = 0; Y : integer = 0); 
```

These functions create a copy of the given operand. There are though a few special considerations regarding the parameters:
  * OnlySubElements: If not set the complete matrix is copied not only the selected sub matrix.
  * AssignSubMatrix with the params x, y: x and y are meant here as column row offsets of the destination matrix meaning if your destination matrix is 5x5 and you assign the submatrix by `mt1.AssignSubMatrix(mt0, 2, 2)` your destination matrix memory from mt1[2,2] to mt1[4,4] is overwritten by mt0. Note that mt0 here needs to be a 2x2 matrix and the indices start with 0.

Row and column wise operations are quite similar to the _AssignSubMatrix_ calls but affect only rows and columns of the destination matrix.

```
procedure SetRow(row : integer; const Values : Array of Double);
procedure SetRow(row : integer; Values : TDoubleMatrix; ValRow : integer = 0); 
procedure SetRow(row : integer; Values : IMatrix; ValRow : integer = 0); 
procedure SetColumn(col : integer; const Values : Array of Double);     procedure SetColumn(col : integer; Values : TDoubleMatrix; ValCols : integer = 0); 
procedure SetColumn(col : integer; Values : IMatrix; ValCols : integer = 0); 
```

_SetRow_ and _SetColumn_ here can also be applied on matrices. Here the first column/row is used in the copying process. With the parameter _ValCols/ValRow_ one can offset the origin matrix - basically use it as column/row offset to not always copy the first column/row.

### Examples ###

Assign a full and constant matrix:

```
const mtx : Array[0..5] of double = (1, 2, 3, 4, 5, 6);
      mty : Array[0..3] of double = (0, -1, -2, -3);
      mtDest : Array[0..5] of double = (1, 0, -1, 4, -2, -3);
var mt1 : IMatrix;
    mt2 : IMatrix;
begin
     mt1 := TDoubleMatrix.Create;
     // creates a 3x2 matrix and assigns the above values
     mt1.Assign(mtx, 3, 2);


     // assign only a submatrix
     mt2 := TDoubleMatrix.Create;
     mt2.Assign(mty, 2, 2);

     // overwrite the elements of mt1
     mt1.AssignSubMatrix(mt2, 1, 0);

     // mt1 now should look like: (1, 0, -2,
     //                            4, -2, -3);
     Check(CheckMtx(mt1.SubMatrix, mtDest));
end;
```

Loading images and store them as rows:

```
procedure TestRow;
var mtx : IMatrix;
    mtImg : IMatrix;
const imgW, imgH : Integer = 480;
begin
     // reserve memory to store 10 "virtual" images
     mtx := TDoubleMatrix.Create(10, imgW*imgH);

     for i := 0 to 9 do
     begin
          // load image and convert to matrix
          ...
          // 
          mtImg := TDoubleMatrix.Create(imgW, imgH);
          // convert the image to a single row so we can use it for PCA
          mtImg.ReshapeInPlace(1, imgW*imgH);
          // set the row
          mtx.SetRow(i, mtImg);
     end;

     // now do whatever you want with the converted images e.g. PCA
end;
```