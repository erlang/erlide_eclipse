package org.erlide.wrangler.refactoring.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.text.edits.DeleteEdit;
import org.eclipse.text.edits.InsertEdit;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.ReplaceEdit;
import org.eclipse.text.edits.TextEdit;
import org.incava.util.diff.Diff;
import org.incava.util.diff.Difference;

/**
 * Tool for comparing the refactor tool's output with the original file.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class ComparerTool {

	static private File inFile;
	// static private File outFile;

	static Diff algorithm;
	static private ArrayList<Character> inFileCharArray;
	static private ArrayList<Character> outFileCharArray;
	static private List<Difference> differencesList;

	/**
	 * Converts the given char array to list.
	 * 
	 * @param charArray
	 * @return list of chars
	 */
	private static ArrayList<Character> convertArrayToArrayList(char[] charArray) {
		ArrayList<Character> result = new ArrayList<Character>();
		for (char c : charArray) {
			result.add(c);
		}
		return result;
	}

	/**
	 * Creates <code>Edit</code> object from a <code>Difference</code>
	 * object
	 * 
	 * @param diff
	 * @return
	 */
	private static TextEdit createEditFromDiff(Difference diff) {
		TextEdit result = null;

		// delete
		if (diff.getAddedEnd() == -1 && diff.getDeletedEnd() != -1) {
			result = new DeleteEdit(diff.getDeletedStart(), diff
					.getDeletedEnd()
					- diff.getDeletedStart() + 1);
		}
		// replace
		else if (diff.getAddedEnd() != -1 && diff.getDeletedEnd() != -1) {
			result = createReplaceEdit(diff.getAddedStart(),
					diff.getAddedEnd(), diff.getDeletedStart(), diff
							.getDeletedEnd());
		}
		// insert
		else if (diff.getAddedEnd() != -1 && diff.getDeletedEnd() == -1) {
			result = new InsertEdit(diff.getDeletedStart(), getString(diff
					.getAddedStart(), diff.getAddedEnd()));
		}

		return result;
	}

	// @SuppressWarnings("unchecked")
	// static public ArrayList<TextEdit> createEdits(File in, File out)
	// throws IOException {
	// inFile = in;
	// outFile = out;
	//
	// ArrayList<TextEdit> edits = new ArrayList<TextEdit>();
	// inFileCharArray = null;
	// outFileCharArray = null;
	//
	// inFileCharArray = splitFile(inFile);
	// outFileCharArray = splitFile(outFile);
	//
	// algorithm = new Diff(inFileCharArray, outFileCharArray);
	//
	// differencesList = algorithm.diff();
	// for (Difference d : differencesList) {
	// edits.add(createEditFromDiff(d));
	// }
	//
	// return edits;
	// }

	/**
	 * Readsthe input file, compares with the given new string, then creates
	 * Eclipse's <code>TextEdit</code>-s.
	 */
	static public ArrayList<TextEdit> createEdits(File in, String out)
			throws IOException {
		inFile = in;

		ArrayList<TextEdit> edits = new ArrayList<TextEdit>();
		inFileCharArray = null;
		outFileCharArray = null;

		inFileCharArray = readFile(inFile);
		outFileCharArray = new ArrayList<Character>();
		outFileCharArray = convertArrayToArrayList(out.toCharArray());

		algorithm = new Diff(inFileCharArray, outFileCharArray);

		differencesList = algorithm.diff();
		for (Difference d : differencesList) {
			edits.add(createEditFromDiff(d));
		}

		return edits;
	}

	/**
	 * Creates a <code>ReplaceEdit</code> object from the given parameters and
	 * the stored input/output strings.
	 * 
	 * @param addedStart
	 * @param addedEnd
	 * @param deletedStart
	 * @param deletedEnd
	 * @return
	 */
	private static TextEdit createReplaceEdit(int addedStart, int addedEnd,
			int deletedStart, int deletedEnd) {
		TextEdit result = new MultiTextEdit();

		int addedLength = addedEnd - addedStart + 1;
		int deletedLength = deletedEnd - deletedStart + 1;
		int minLength = Math.min(addedLength, deletedLength);

		if (deletedLength < addedLength) {
			result.addChild(new InsertEdit(deletedStart + minLength, getString(
					addedStart + minLength, addedEnd)));
		}

		result.addChild(new ReplaceEdit(deletedStart, minLength, getString(
				addedStart, addedStart + minLength - 1)));

		if (addedLength < deletedLength) {
			result.addChild(new DeleteEdit(deletedStart + minLength,
					deletedLength - minLength));
		}

		return result;
	}

	/**
	 * Gets the string from the new source file, according to the given
	 * parameters.
	 * 
	 * @param from
	 *            string's starting position
	 * @param to
	 *            string's ending position
	 * @return
	 */
	private static String getString(int from, int to) {
		String s = "";
		// from, to+1
		for (char c : outFileCharArray) {
			s += c;
		}
		return s.substring(from, to + 1);
	}

	/**
	 * Read the given files into a list of <code>Character</code>-s.
	 * 
	 * @param file
	 *            the file which is read
	 * @return
	 * @throws IOException
	 *             if any i/o error occurs this exception is raised.
	 */
	static private ArrayList<Character> readFile(File file) throws IOException {
		ArrayList<Character> result = new ArrayList<Character>();
		BufferedReader input = new BufferedReader(new FileReader(file));

		String line;
		while ((line = input.readLine()) != null) {
			char[] chars = line.toCharArray();
			for (int i = 0; i < chars.length; ++i) {
				result.add(chars[i]);
			}
			result.add('\n');
		}
		input.close();
		if (result.size() != 0) {
			result.remove(result.size() - 1);
		}

		return result;
	}

	private ComparerTool() {
	}

}
