# Persistence #

Persistence has been implemented for easy loading and storing of
matrices and other data. The persistence framework would support all kinds of formats but only one binary format has been implemented yet.

## Storing of data ##

Each object which is capable of writing itself to a stream implements
the **IMathPersistence** interface:

```
IMathPersistence = interface
    ['{16A5F1D0-CA39-4BD0-BABE-E1E70B0045C3}']
    procedure SaveToFile(const FileName : string; Writer : TCustomMathPersistenceIOClass);
    procedure SaveToStream(stream : TStream; Writer : TCustomMathPersistenceIOClass);
  end;
```

For now there only exists the `TBinaryReaderWriter` class which has to be
used as second parameter.

## Loading of data ##

There are two functions to load an object from a file:

```
function ReadObjFromFile(const FileName : TFileName) : TBaseMathPersistence;
function ReadObjFromStream(stream : TStream) : TBaseMathPersistence;
```

The resulting object has to be casted to the expected class afterwards!

Note you need to have the `BinaryReaderWriter` unit included which automatically registers itself for the IO subsystem and is responsible for the data reading/writing and the compression.

Note that the loading and storing of data is independent of the file extension!

## Examples ##

Storing a matrix to a file:

```
  mx := TDoubleMatrix.Create(1, 1);
  mx.SaveToFile('Test.dat', TBinaryReaderWriter);
```

or a more verbose example

```
procedure TestPersistence;
var dx, dy : Array[0..49] of double;
    i : Integer;
    mx, my : TDoubleMatrix;
begin
     for i := 0 to High(dx) do
     begin
          dx[i] := i + 1;
          dy[i] := i + 2;
     end;

     mx := TDoubleMatrix.Create;
     with TBinaryReaderWriter.Create do
     try
        mx.Assign(dx, 5, 10);
        SaveToFile(mx, 'matrixData1.txt');
        mx.Assign(dy, 5, 10);
        SaveToFile(mx, 'matrixData2.txt');
     finally
            Free;
     end;

     mx.Free;
     my.Free;
end;
```

Reading a matrix from a file

```
   // my is the matrix from the verbose example
   my := ReadObjFromFile('matrixData1.txt') as TDoubleMatrix;
```

## Implementing your own data format ##

tbd.