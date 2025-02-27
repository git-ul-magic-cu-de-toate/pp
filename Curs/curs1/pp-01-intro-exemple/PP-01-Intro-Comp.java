package javaplay;

import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

@SuppressWarnings("javadoc")
public class PPExample
{
	// închideri funcționale
	@SuppressWarnings("static-method")
	public Comparator<PPExample> f(int arg)
	{
		final int x = arg;
		
		return new Comparator<PPExample>() {
			@Override
			public int compare(PPExample o1, PPExample o2)
			{
				return x; // eroare: x nu va mai fi disponibil dupa iesirea din f dacă x nu este declarat ca final
			}
		};
	}
	
	
	// perspectivă imperativă vs funcțională
	static {	// Java 7
		List<PPExample> collection7 = new LinkedList<>();
		// ...
		Collections.sort(collection7, new Comparator<PPExample>() {
			@Override
			public int compare(PPExample t1, PPExample t2) {
				// ...
				return 1;
			}
		});
		
		for(int i = 0; i < collection7.size(); i++)	// NU NU NU NU NU NU NU
			System.out.println(collection7.get(i));
		
		for(PPExample t : collection7)
			System.out.println(t);
	}
	
	
	static {
		List<PPExample> collection8 = new LinkedList<>();
		Collections.sort(collection8, (PPExample t1, PPExample t2) -> { return 1; });
		collection8.forEach(t -> System.out.println(t));
	}
}
