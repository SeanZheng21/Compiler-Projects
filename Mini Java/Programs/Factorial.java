class Factorial {
    public static void main(String[] a) {
        System.out.println(new Fac().computeFac(10));
        //        System.out.println(new Fac().loop(1000000, 1000));
    }
}

class Fac {
    public int computeFac(int num) {
        int numAux;
        if (num < 1) numAux = 1;
        else numAux = num * (this.computeFac(num-1));
        return numAux;
    }
    // public int loop(int n, int num) {
    //     int i;
    //     int res;
    //     i = 0;
    //     res = 0;
    //     while (i < n) {
    //         res = res + this.computeFac(num);
    //         i = i + 1;
    //     }
    //     return res;
    // }

}
