if [ "$#" -ne 1 ] 
    then echo "Wrong number of parameters. 1 expected, $# provided."
else ghc -O2 --make -lstdc++ -lzmq mc2mabs_c.cpp $1.cpp MC2MABS.lhs -o $1
fi

