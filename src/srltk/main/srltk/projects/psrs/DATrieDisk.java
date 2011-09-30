package srltk.projects.psrs;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.LinkedList;
import java.util.TreeMap;
public class DATrieDisk extends DATrie
{
  public static boolean DA_DISK_CACHE = false;
  public static int DA_DISK_CACHE_READAHEAD = 10;
  public static int DA_DISK_CACHE_MAX = 100000;

  private RandomAccessFile base;
  private int base_max_fp;
  private RandomAccessFile check;
  private TreeMap<Integer, Integer> check_buffer,base_buffer,address_buffer;
  private LinkedList<Integer> history_check,history_base,history_address;
  private File file_base,file_check;
  private int check_max_fp;
    private int check_cell_size = 4; //bytes per cell
  private int base_cell_size = 8; //bytes per cell
  
  private boolean buffering_enabled;
  private int read_ahead;
  int misses;
  int hits;
  public DATrieDisk(String filename_base, String filename_check) throws IOException
  {
    this(filename_base,filename_check,false);
  }
  
  public DATrieDisk(String filename_base, String filename_check, boolean delete_if_exists) throws IOException
  {    
    super();
    this.hits = 0;
    this.misses = 0;
    file_base = new File(filename_base);
    file_check = new File(filename_check);
    this.base_buffer = new TreeMap<Integer,Integer>();
    this.check_buffer = new TreeMap<Integer,Integer>();
    this.address_buffer = new TreeMap<Integer,Integer>();
    this.history_check = new LinkedList<Integer>();
    this.history_base = new LinkedList<Integer>();
    this.history_address = new LinkedList<Integer>();
    buffering_enabled = DA_DISK_CACHE;
    read_ahead = DA_DISK_CACHE_READAHEAD;
    if(read_ahead <= 0)
      buffering_enabled = false;
    boolean files_exist = false;
   
    if(file_base.exists() && file_check.exists())
    {
      
      if(delete_if_exists)
      {
        file_base.delete();
        file_check.delete();
      }
      else
        files_exist = true;
    }
    else if((file_base.exists() && !file_check.exists()) ||(!file_base.exists() && file_check.exists()))
    {
     System.err.println("Error: only one of the two files exists");
     System.exit(1);
    }
    
    base = new RandomAccessFile(file_base,"rw");
    check = new RandomAccessFile(file_check,"rw");    

    
    //get size of files
    if(files_exist)
    {
      try
      {        
        array_size = (int)(base.length()/base_cell_size);
        if(array_size <= INITIAL_ARRAY_SIZE)
        {          
          initializeFiles(INITIAL_ARRAY_SIZE);
        }
        else
        {
          
          check_max_fp = (int)check.length();
          base_max_fp = (int)base.length()-4;
          //initialize buffers
          readCheck(0);
          readBase(0);          
        }
        
      } catch (IOException e)
      {
        e.printStackTrace();
        System.exit(1);
      }
    }
    else
    {
      
      initializeFiles(INITIAL_ARRAY_SIZE);
    }
  }

  private void initializeFiles(int size) throws IOException
  {
    array_size = size;
    base.seek(0);
    check.seek(0);
    System.out.flush();
    for(int i=0; i<=array_size; i++)
    {
      
      base.writeInt(0);base.writeInt(0);
      check.writeInt(0);
    }
    check_max_fp = (int)check.length();
    base_max_fp = (int)base.length()-4;
  }
  
  protected void setLength(int length) 
  {
    try
    {
      base.seek((length+1)*base_cell_size);
      check.seek((length+1)*check_cell_size);
    } catch (IOException e)
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    
  }
  
  protected void ensureLength(int length)
  {    
    if(length <= array_size)
        return;
    else
      System.out.println("ensuring length "+length);
    try
    {
      //seek end of file
      base.seek((array_size+1)*base_cell_size);
      check.seek((array_size+1)*check_cell_size);
      int size = array_size;
      int new_size = (int)(array_size * ARRAY_LENGTHEN_FACTOR);
      for(int i=size; i<new_size; i++)
      {
        base.writeInt(0);base.writeInt(0);
        check.writeInt(0);      
      }
      array_size = new_size;
      check_max_fp = (int)check.length();
      base_max_fp = (int)base.length()-4;
    }
    catch(IOException e)
    {
      e.printStackTrace();
      System.exit(1);
    }
  }
  
  //--------------------------------------------------------------------------------
  
  protected int read(int state, RandomAccessFile f, 
		  TreeMap<Integer,Integer> buffer, LinkedList<Integer> history, 
		  boolean address) throws IOException
  {	  
	  
	 if(buffering_enabled)
		 history.addLast(state);
    if(buffering_enabled && buffer.containsKey(state))
    {
      this.hits++;
      return buffer.get(state);
    }
    else
    {
      
      int file_loc = 0;
      if(f == check)
        file_loc += state * check_cell_size;
      else if (f==base && !address)
        file_loc += state * base_cell_size;
      else if (f==base && address)
        file_loc += state * base_cell_size+4;
      //seek in the file
      f.seek(file_loc);
      
      if(!buffering_enabled)
      {        
        return f.readInt();
      }
      else
      {
    	  this.misses++;
        int st;
        for(int i=0; i<read_ahead; i++)
        {
          st = state+i;
          if(f==base && address)
          {
            address_buffer.put(st, f.readInt());
            if((st+1)*base_cell_size >= base_max_fp)
            {
              base_buffer.put(st+1, f.readInt());
            }
            else
            {
              break;
            }
          }
          else if(f==base)
          {
            base_buffer.put(st, f.readInt());
            address_buffer.put(st, f.readInt());
            if((st+1)*base_cell_size >= base_max_fp-base_cell_size)
            {
              break;
            }
          }
          else
          {
             check_buffer.put(st, f.readInt());
             if((st+1)*check_cell_size >= check_max_fp- check_cell_size)
             {
               break;
             }
          }
        }
        //System.out.println("I have "+buffer.size()+" items buffered ");
        if(buffer.size() > DA_DISK_CACHE_MAX)
        {
        	buffer.remove(history.getFirst());
        	history.removeFirst();
        	
        }
        return buffer.get(state);
      }

    }
  }
  
  
  //reads a byte from the check file
  protected int readCheck(int state) throws IOException
  {
    return(read(state,check,check_buffer,history_check,false));
  }
  //write v to base in check file
  protected void writeCheck(int state, int v) throws IOException
  {
    check.seek(state*check_cell_size);
    check.writeInt(v);
  }
  //reads a byte from the base file
  protected int readBase(int state) throws IOException
  {
    return(read(state,base,base_buffer,history_base,false));
  }
  //write v to base in base file
  protected void writeBase(int state, int v) throws IOException
  {
    base.seek(state*base_cell_size);
    base.writeInt(v);
  }   
  //read address field
  protected int readAddress(int state) throws IOException
  {
    return(read(state,base,address_buffer,history_address,true));
  }

  protected void writeAddress(int s, int addr) throws IOException
  {    
    base.seek(s * base_cell_size+4);
    base.writeInt(addr);
  }
  
  protected void clear() throws IOException
  {
    check.seek(0);
    for(int i=0; i < array_size; i++)
    {
      writeCheck(i,EMPTY);
      writeAddress(i,EMPTY);
    }
  }
  public void copyFromMemTrie(DATrieMem trie) throws IOException
  {
    int size = trie.getArraySize();    
    for(int i=HEAD; i<size; i++)
    {
     this.writeCheck(i,trie.readCheck(i)); 
     this.writeBase(i,trie.readBase(i)); 
     this.writeAddress(i,trie.readAddress(i));
    }
  }
  public DATrieMem convertToMemTrie() throws IOException
  {
    DATrieMem trie = new DATrieMem();
    boolean buffer_enabled = this.buffering_enabled;
    this.buffering_enabled = false;
    int size = this.getArraySize();
    trie.setLength(size);
    
    
    base.seek(0);
    check.seek(0);
    byte[] b = new byte[(int)base.length()];
    base.readFully(b);
    byte[] c = new byte[(int)check.length()];
    check.readFully(c);
    trie.setLength(c.length/check_cell_size);
    int t;
    for(int i=HEAD; i<size; i++)
    {      
    	int temp = 0;
    	for(int j=0;j<base_cell_size/2;j++)
    	{    		
    		t = b[i*base_cell_size+j];
    		if(j > 0)
    			t &= 0xFF;
    		temp |= (t << (3-j)*8);
    		//System.err.printf("t = 0x%x, j=%d, tshift = 0x%x, temp = 0x%x\n", t,j,t<<(3-j)*8, temp);
    	}   	
//    	int a = readBase(i);
//    	if(a != temp ){
//    		System.err.printf("temp = 0x%x actual is 0x%x",temp,a);
//    		return null;
//
//    	}
    	trie.writeBase(i,temp);
    	temp = 0;
    	for(int j=(base_cell_size/2);j<base_cell_size;j++)
    	{
    		t = b[i*base_cell_size+j];
    		if(j > 4)
    			t &= 0xFF;
    		temp |= (t << (7-j)*8);
    		//System.out.printf("t = 0x%x, j=%d, tshift = 0x%x, temp = 0x%x\n", t,j,t<<(3-j)*8, temp);
    	}

    	trie.writeAddress(i,temp);
     	temp = 0;
    	for(int j=0;j<check_cell_size;j++)
    	{
    		t = c[i*check_cell_size+j]; 
    		if(j > 0)
    			t &= 0xFF;
    		temp |= (t<< (3-j)*8); 
    	}  	
        trie.writeCheck(i,temp);
        
    	
        //
    }
    this.buffering_enabled = buffer_enabled;
    return trie;
  }
  
  
  public void close()
  {
    try
    {
      base.close();
      check.close();
    }
    catch(Exception e){}
  }
}
