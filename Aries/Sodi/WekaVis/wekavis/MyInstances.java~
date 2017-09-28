package wekavis;

import weka.core.*;
import java.io.*;

//A version of the Datatype that holds the data for Weka.  This was so that
//  the header could be set and copied from a dataset holding all the fields and values

class MyInstances extends weka.core.Instances
{
	public MyInstances(Reader reader) throws IOException {
		super(reader);
	}

	public MyInstances(Reader reader, int capacity) throws IOException {
		super(reader,capacity);
	}

	public MyInstances(Instances dataset) {
		super(dataset);
	}

	public MyInstances(Instances dataset, int capacity) {
		super(dataset,capacity);
	}

	public MyInstances(Instances source, int first, int toCopy) {
		super(source,first,toCopy);
	}

	public MyInstances(String name, FastVector attInfo, int capacity) {
		super(name,attInfo,capacity);
	}

	public void setHeader(FastVector attributeInfo) {
		m_Attributes = attributeInfo;
	}

	public FastVector getHeader() {
		return m_Attributes;
	}
}
