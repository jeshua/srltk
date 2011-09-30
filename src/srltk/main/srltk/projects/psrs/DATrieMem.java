package srltk.projects.psrs;

import java.io.IOException;
import java.util.TreeMap;

public class DATrieMem extends DATrie
{
  protected int[] base;
  protected int[] check;
  private TreeMap<Integer,Integer> data_addresses;
  private int head;
  
  public DATrieMem(){
    this(4,249);
  }
  public DATrieMem(int minChar, int maxChar)
  {    
    super(minChar,maxChar);
    array_size = super.INITIAL_ARRAY_SIZE;
    base = new int[(int)array_size];
    check = new int[(int)array_size];
    data_addresses = new TreeMap<Integer, Integer>();
    head = 1;
    base[head] = 1;
    check[base[head]] = head;
    children = new int[maxChar];
    super.Init();
  }     

  protected void setLength(int length)
  {
    lengthenArrays(length);
  }
  
  protected void ensureLength(int length)
  {
    while(array_size < length)
    {
      lengthenArrays((int)(array_size*ARRAY_LENGTHEN_FACTOR));
    } 
  }

  protected int readBase(int s)
  {
    return base[s];
  }
  
  protected int readCheck(int s)
  {
    return check[s];
  }
  protected void writeBase(int s, int v) 
  {
   base[s] = v;    
  }
  protected void writeCheck(int s, int v)
  {
    check[s] = v;    
  }
  protected int readAddress(int s)
  {
    if(data_addresses.containsKey(s))
    {
      return data_addresses.get(s);
    }
    else
    {
      return EMPTY;
    }
  }
  protected void writeAddress(int s, int target)
  {
    if(target != EMPTY)
      data_addresses.put(s, target);
  }
  
 //----------------------------------------------------------------------------
  
  private void lengthenArrays(int new_size)
  {    
    int[] new_base = new int[(int)(new_size)];
    int[] new_check = new int[(int)(new_size)];
    for(int i=0; i<array_size; i++)
    {
      new_base[i] = base[i];
      new_check[i] = check[i];
    }   
    base = new_base;
    check = new_check;
    this.array_size = new_size;
  }

  //----------------------------------------------------------------------------

  public void printDensity()
  {
    double free = 0;
    int used = 0;
    for(int i=0; i<array_size;i++)
    {
      if(check[i] == EMPTY)
        free+=1;
      else
        used+=1;
    }
    System.out.println("used: " + used);
    System.out.println("free: " + (int)free);
    System.out.println("total size: "+array_size);
    System.out.println("Density: "+(1-(free/array_size)));
  }
  
  

  public static void main(String[] args) throws IOException
  {
    DATrieMem test = new DATrieMem();
    
    test.insert(new int[]{1,2,5,2},14);
    test.insert(new int[]{1,2,2},19);
    test.insert(new int[]{1,5,2},9);
    test.insert(new int[]{2,5,2},3);
    test.insert(new int[]{1,2,5},8);
    
    System.out.println(test.find(new int[]{1,2,2}));
    test.getSomeLeaves(test.HEAD, 5);
    test.display();
  }
}
