# Universal Turing Machine

Three kinds of Turing machine(CLASSICAL,LRTM,BBTM) was built.
There is two modes to show them, M1 only shows result only, M2 show the animation 
Here are the docker commands about how to run them.

## 1.Build Docker Image Or Load Docker Image

Build Docker Image: 
```
docker build -t utm-haskell . 
```

Load Docker Image: 
```
docker load -i image-10.tar
```

## 2.Run Commands:

### CLASSICAL:
```
docker run --rm utm-haskell test.desc 0101 M1
docker run --rm -it utm-haskell test.desc 0101 M2 
```

### LRTM
```
docker run --rm utm-haskell LRTM.desc 111111 M1
docker run --rm -it utm-haskell LRTM.desc 111111 M2
```

### BBTM
```
docker run --rm utm-haskell BBTM.desc 000 M1
docker run --rm -it utm-haskell BBTM.desc 000 M2 
```

## 3.Save Docker Image
```
docker save -o image-10.tar utm-haskell
```
