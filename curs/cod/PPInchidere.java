import java.util.Comparator;

@SuppressWarnings("javadoc")
public class PPInchidere
{
	private String field = "test";
	
	public Comparator<PPInchidere> f(String arg)
	{
		String x = arg + field;
		
		return new Comparator<PPInchidere>() {
			@Override
			public int compare(PPInchidere o1, PPInchidere o2)
			{
				field = o1.toString(); // în Java 1.7 - warning pentru că field este private și aici suntem în altă clasă
//				return 0;
				return x.length(); // în Java 1.7 - eroare: x nu va mai fi disponibil dupa iesirea din f
			}
		};
	}
}
