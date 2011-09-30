package srltk.projects.psrs;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.Random;
import java.util.Stack;

public abstract class DATrie implements Comparable<DATrie>
{
  public enum RelocMethod {BRUTE_FORCE,RANDOM};
  //PARAMS
  public RelocMethod RELOC_METHOD = RelocMethod.BRUTE_FORCE;
  protected final double RANDOM_BASE_MAX = .9;//percentage of array size when picking random base
  protected final double ARRAY_LENGTHEN_FACTOR = 2;//double arrays when they need to be lengthened
  protected final double RELOCATION_BOREDOM = .0001;//proportion of array_list until choosing random base
  protected final double RELOCATION_LENGTHEN_THRESH = .6;//proportion of array_list before lengthening array
  protected final int INITIAL_ARRAY_SIZE = 1000;
    
  public static final int FAIL = -1;
  public static final int EMPTY = 0;  
  public static final int HEAD = 1;
  public int w = 0;

  private int minChar;
  private int maxChar;
    
  protected int array_size;
  protected int[] children;
  protected Random random;
  protected byte[] empty_slots;//bit for each empty slot
  private boolean use_empty_array;
  public DATrie(){
    this(5,100);
  }

  public DATrie(int minChar, int maxChar)
    {
      this.minChar = minChar;
      this.maxChar = maxChar;
      children = new int[maxChar];
      random = new Random();
      empty_slots = new byte[100];
      use_empty_array = false;
    }
  
  public void Init()
  {
    this.ensureEmptySize();
  }

  
  protected boolean empty(int b) throws IOException
  {
    if(use_empty_array)
      {      
        byte mask = (byte) (1<<(b%8));    
        return (empty_slots[b/8] & mask)!=0;
      }
    else
      {
        return readCheck(b)==EMPTY;
      }
  }
  
  protected void setEmpty(int b, boolean empty)
  {
    if(use_empty_array)
      {
        byte mask = (byte) (1<<(b%8));
        if(empty)    
          empty_slots[b/8] &= ~mask;
        else
          empty_slots[b/8] |= mask;
      }
  }
  
  protected void ensureEmptySize()
  {
    if(use_empty_array && empty_slots.length < array_size/8)
      {
        byte[] temp = empty_slots;
        empty_slots = new byte[(int)Math.ceil(array_size/8.0)];
        for(int i=0; i<temp.length;i++)
          {
            empty_slots[i] = temp[i];
          }
      }
  }  
    
  protected int getEmptyBase(int start_base) throws IOException
  {
    int b = start_base;
    while(!empty(b))
      {
        if(b >= array_size - maxChar)
          b = 1;
        else
          b++;      
      }
    return b;
  }
  
  /**
   * Get a base where we can put a new state
   * @return
   * @throws IOException 
   */
  protected int getRandomBase() throws IOException
  {
    int b;
    b = random.nextInt((int)(array_size * RANDOM_BASE_MAX));
    return getEmptyBase(b);
  }
  
  
  /**
   * Write to the check file. 
   * @param s - state
   * @param v - owner
   * @throws IOException
   */
  private void writeCheckW(int s, int v) throws IOException
  {
    this.writeCheck(s,v);
    setEmpty(s,v==EMPTY);
  }
  
  
  /**
   * Ensure the arrays are long enough. 
   * The wrapper method ensureLengthW extends the empty_slots array if necessary
   * @param length
   */
  protected abstract void ensureLength(int length);  
  private void ensureLengthW(int length)
  {
    this.ensureLength(length);   
    this.ensureEmptySize();
  }
  protected abstract void setLength(int length);  

  
  
  /**
   * Check if the state reached by following c is owned by s
   * @param s start state
   * @param c transition
   * @return
   * @throws IOException
   */
  protected boolean owned(int s, int c) throws IOException
  {    
    return readCheck(readBase(s)+c)==s;
  }  
  
  /**
   * Move state to new base and resolve children. This function is part of resolveConflict
   * which find a safe new base. NOTE: if the new base is not safe then this function may 
   * mess up the tree.
   * @param state
   * @param new_base
   */ 
  protected void relocateState(int state, int new_base) throws IOException  
  {    
    getChildren(state);
    int statebase = readBase(state);
    for(int c = minChar; c < maxChar; c++)
      {       
        if(children[c]==EMPTY) continue;
      
        //update children
        int old_child = statebase+c;      
        int new_child = new_base+c;
        //set owner of new location
        writeCheckW(new_base + c,state);
     
        //copy the node
        writeBase(new_child , readBase(old_child));
     
        for(int d  = minChar; d < maxChar; d++)
          {
            if(owned(old_child,d))
              writeCheckW(readBase(old_child)+d, new_base+c);
          }
        writeCheckW(old_child,EMPTY);
      
        //move data address for child
        writeAddress(new_child,readAddress(old_child));
        writeAddress(old_child,EMPTY);
      }
    writeBase(state,new_base);
  }
  /**
   * Finds a safe base to move state to. This means that the new base is
   * empty and the children of state fit as offsets from base.  
   * @param state
   * @param required_child in addition to current children, ensure space for this child
   * @return
   */

  protected int findSafeBase(int state, int required_child) throws IOException
  {
    getChildren(state);//sets children variable
    children[required_child] = 1;//just some number > 0
    int loops = 0;
    
    int b;
    if(this.RELOC_METHOD==RelocMethod.BRUTE_FORCE)
      {
        b = readBase(state)-array_size/5;//getBase();{
      }
    else
      {
        b = getRandomBase();
      }
    if(b < 1)
      b = 1;
    //loop until we find a safe base
    while(true)
      {    
        if(checkSafeBase(state,b))
          return b;
        else
          {
            //bookeeping
            loops++;
            //choose next base
            if(b + maxChar >= array_size)
              b = 1;
            else
              b++;
            if(RELOC_METHOD==RelocMethod.RANDOM)
              {
                if((loops % (array_size * RELOCATION_BOREDOM)) == 0)
                  b = getRandomBase();
              }
            //lengthen arrays if we loop too long
            if(loops > array_size * RELOCATION_LENGTHEN_THRESH)
              {        
                //System.out.println("I have looped "+loops+" times, resizing");
                b = array_size;
                ensureLengthW((int)(array_size*this.ARRAY_LENGTHEN_FACTOR));
                return b;//just put this entry at the end of the old array size        
              }
          }
      }
  }

  /**
   * Checks if a state can fit at base. ASSUMES getchildren was called 
   * @param state
   * @param base
   * @return
   * @throws IOException
   */
  private boolean checkSafeBase(int state, int base) throws IOException
  {
    //loop through children that aren't 0
    boolean safe = true;
    for(int c=minChar;c<maxChar;c++)    
      {
        if(children[c] > 0)
          {
            //make sure we haven't gone out of bounds
            if(base >= array_size || base+c >= array_size)
              {
                safe = false;
                break;
              }
            //check if this new base CAN'T fit this child
            if(!empty(base+c))
              {       
                safe = false;
                break;
              }
          }
      }
    return safe;
  }

  
  /**
   * Follow transition from state and retrieve the character.
   * Returns EMPTY if the state at the end of the transition is unset
   * Returns FAIL if the state at the end of the transition is owned by another state
   * @param state
   * @param character
   * @return child node or FAIL or EMPTY
   */
  protected int getChild(int state, int character) throws IOException
  {
    ensureLengthW(state);
    int t = readBase(state) + character;
    int  check = readCheck(t);
    if(check == state)
      return t;
    else if(check == EMPTY)
      return EMPTY;
    else
      return FAIL;  
  }

  /**
   * Adds a child of state by following transition c.
   * @param state start state
   * @param c transition to follow
   * @return
   */
  protected int addChild(int state, int c) throws IOException
  {         
    int stateb = readBase(state);
    int b = getEmptyBase(stateb);
    int child = stateb + c;
    writeBase(child,b);
    writeCheckW(child,state);
    return child;
  }
   
  
  
  /**
   * Insert string into Trie
   * @param ar - array of integers to insert
   * @param address - address associated with this string
   * @throws IOException
   */
  public void insert(int[] ar, int address) throws IOException
  {        
    int state = HEAD;
    int last_state;
    for(int i=0; i<ar.length; i++)
      {
        last_state = state;  
        state = (int)getChild(state,ar[i]);
        if(state == FAIL)
          {
            int new_base = findSafeBase(last_state,ar[i]);
            relocateState(last_state,new_base);
            state = (int)addChild(last_state,ar[i]);
          }
        else if(state == EMPTY)
          {
            state = (int)addChild(last_state,ar[i]);
          }       
      }
    writeAddress(state,address);
  } 
  
  /**
   * Return state by following string.
   * @param ar - array to search for
   * @return state associated with string
   * @throws IOException 
   */
  public int find(int[] ar) throws IOException
  {
    return find(ar,this.HEAD);
  }
  public int find(int[] ar, int state) throws IOException
  {
    int length = ar.length;
    int c;
    for(int i=0; i<length; i++)
      {
        state = getChild(state, ar[i]);
        if(state == FAIL || state==EMPTY)
          return EMPTY;
      }
    return state;
  }
  /**
   * Returns the address of a state.
   * @param state
   * @return
   * @throws IOException
   */
  public int getAddress(int state) throws IOException
  {
    return readAddress(state);    
  }
  

  /**
   * Get list of children from state and set member variable
   * @param s
   * @return
   * @throws IOException 
   */
  protected final int[] getChildren(int state) throws IOException
  {    
    int child;
    for(int i=minChar; i<maxChar;i++)
      {
        child = readBase(state)+i;
        if(child >= array_size)
          {
            for(int j=i; j<maxChar;j++)
              children[j] = EMPTY;
            break;
          }      
        if(readCheck(child) == state)
          {    
            children[i] = child;
          }
        else
          {
            children[i] = EMPTY;
          }
      }
    return children;
  }

  
  public ArrayList<Integer> getSomeLeaves(int st, int max) throws IOException
  {
    return getSomeLeaves(st,max,-1,-1);
  }
  public ArrayList<Integer> getSomeLeaves(int st, int max,long time_start,long cutoff_time) throws IOException
  {
    boolean check_time = false;
    if(time_start > 0)
      check_time = true;
    ArrayList<Integer> ret = new ArrayList<Integer>();
    Stack<Integer> stack = new Stack<Integer>();
    ArrayList<Integer> c = new ArrayList<Integer>();  
    stack.add(st);
    int addr;
    int count = 0;
    Integer state = st;
    while(stack.size()>0)
      {
        state = stack.pop();
        if((addr = readAddress(state))!=EMPTY)
          {
            ret.add(addr);
            count++;
          }
        this.getChildren(state);
        c.clear();
        for(int i=1; i<children.length; i++)
          {			  
            if(children[i] != EMPTY) 
              {
                c.add(children[i]);
              }
          }
        //Collections.shuffle(c);
        for(int i=0; i<c.size();i++)
          {
            stack.push(c.get(i));        
          }		  
        if(count >= max)
          break;
        if(check_time)
          if(System.currentTimeMillis() - time_start > cutoff_time)
            {
              //System.out.println("overtime");
              break;
            }
      }
    return ret;    
  }

  /**
   * Display using dotty
   */
  public void display() 
  {
    File temp;
    BufferedWriter out;
    int count = 0;
    int max = 100;
    try
      {
        temp = File.createTempFile("disp",".dot");
        out = new BufferedWriter(new FileWriter(temp));
        //create the graph
        String digraph = "digraph dispgraph { \n";
        //BFS across trie
        int state;
        LinkedList<Integer> queue = new LinkedList<Integer>();
        queue.add(HEAD);

        while(!queue.isEmpty())
          {
            state = queue.poll();
            final int[] children = getChildren(state);
            for(int i=0; i<children.length; i++)
              {
                if(children[i] > 0)
                  {
                    if(count++ > max) break;
                    digraph += "\""+state+","+readAddress(state)+"\" -> \""+children[i]+","+readAddress(children[i])+"\"[label=\""+i+"\"]\n";
                    queue.add(children[i]);
                  }
              }    
            if(count > max) break;
          }
        digraph+="}\n";

        System.out.println(digraph);
        out.write(digraph);
        //display the graph
        out.flush();
        String tmp_filename = temp.toString();
        String command = "/usr/bin/dot -Tps "+tmp_filename+" -o "+tmp_filename+".ps";
        //System.out.println(command);
        Process p = Runtime.getRuntime ().exec (command);


        //wait for dotty to close
        try{p.waitFor();} catch (InterruptedException e)
          {e.printStackTrace();}

        command = "evince "+tmp_filename+".ps";
        //System.out.println(command);
        p = Runtime.getRuntime ().exec (command);

        try{p.waitFor();} catch (InterruptedException e)
          {e.printStackTrace();}
        temp.delete();
        out.close();
      } catch (IOException e1)
      {
        // TODO Auto-generated catch block
        e1.printStackTrace();
      }
  }
  
  
  /*--------------------------------------------------------------*/

  public int getArraySize() { return array_size;};
  
  /*--------------------------------------------------------------*/
  
  protected abstract int readBase(int s) throws IOException;
  protected abstract void writeBase(int s, int v) throws IOException;
  protected abstract int readCheck(int s) throws IOException;  
  protected abstract void writeCheck(int s, int v) throws IOException;
  protected abstract int readAddress(int s) throws IOException;
  protected abstract void writeAddress(int s, int target) throws IOException;
  protected void close() { };
  
  
  public int compareTo(DATrie t)
  {
    if(w > t.w)
      return 1;
    else if (w < t.w)
      return -1;
    else 
      return 0;
  }
  
  public void disp(){
    for(int i=HEAD; i<60; i++)
      {
        try
          {
            System.out.printf("%d\t[%d %d]\t[%d]\n",i, this.readBase(i),this.readAddress(i),this.readCheck(i));
          } catch (IOException e)
          {
            e.printStackTrace();
          }
      }
  }
}
