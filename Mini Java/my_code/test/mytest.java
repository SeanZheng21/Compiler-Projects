class mytest {
    public static void main(String[] args) {
        {
            System.out.println(42);
            System.out.println(/*great program*/52);
        
            while (52) {
                System.out.println(42);
            }
            if (52)
                System.out.println(42);
            else
                System.out.println(52);

            i = 52;
            arr[this] = 52;
            arr[52] = 52;
            arr[arr] = 52;
            arr[!arr] = 52; 
            arr[new foo()] = 52;
            arr[new int [true]] = 52;
            arr[true && false] = 52;
        }
    }
}
