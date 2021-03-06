package uu.jazy.core ;

/**
 * Lazy and Functional.
 * Package for laziness and functions as known from functional languages.
 * Written by Atze Dijkstra, atze@cs.uu.nl
 */

/**
 * An application of a Eval to 5 parameters.
 */
class Apply5 extends Apply
{
	//private static Stat statNew = Stat.newNewStat( "Apply5" ) ;
	
	protected Object p1, p2, p3, p4, p5 ;
	
	public Apply5( Object f, Object p1, Object p2, Object p3, Object p4, Object p5 )
	{
		super( f ) ;
		this.p1 = p1 ;
		this.p2 = p2 ;
		this.p3 = p3 ;
		this.p4 = p4 ;
		this.p5 = p5 ;
		//statNew.nrEvents++ ;
	}
	
    protected void eraseRefs()
    {
    	//function = null ;
    	p1 = p2 = p3 = p4 = p5 = null ;
    }
    
    public Object[] getBoundParams()
    {
	    if ( p1 == null )
	        return Utils.zeroArray ;
	    return new Object[] {p1,p2,p3,p4,p5} ;
    }

    public int getNrBoundParams()
    {
        return 5 ;
    }

}
