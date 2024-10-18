
/**
 * Write a description of class BubbleSort here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
public class BubbleSort
{
    
    private int listt[];

    /**
     * Constructor for objects of class BubbleSort
     */
    public BubbleSort(int []listt)
    {
        this.listt = listt ; 
    }
    
    public int [] listtGet (){
        return listt;
    }
    
    public int [] listtSet (){
        if (listt == null) throw new IllegalArgumentException ("lista vacia");
        if (listt.length == 1) throw new IllegalArgumentException ("tiene que tener mas de 1 elemento");
        int i;
        int j;
        for (i=0 ; i<listt.length; i++){
            for (j=1 ; j<listt.length-i;j++){
                if (listt[j-1]>listt[j]){
                   int temp = listt[j];
                   listt [j] = listt[j-1];
                   listt [j-1] = temp;
                }
            }
        }
        assert (true);
        return listt; 
    }
    
}
