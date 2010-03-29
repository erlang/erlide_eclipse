package org.erlide.ui.internal;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.part.ViewPart;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.core.util.Tuple;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class MonitorView extends ViewPart {
	private final List<MonitorEntry> entries;
	private TreeViewer viewer;
	private Button button;

	public MonitorView() {
		entries = new ArrayList<MonitorEntry>();
	}

	@Override
	public void createPartControl(final Composite parent) {
		final Composite composite = new Composite(parent, SWT.NONE);
		composite.setLayout(new GridLayout(2, false));
		boolean devMode = ErlideUtil.isDeveloper();
		{
			button = new Button(composite, SWT.NONE);
			button.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(final SelectionEvent e) {
					FileDialog fd = new FileDialog(composite.getShell(),
							SWT.OPEN);
					fd.setText("Open log file");
					fd.setFilterPath(ErlideUtil.getReportLocation());
					fd.setFilterExtensions(new String[] { "*.txt" });
					final String selected = fd.open();

					if (selected != null) {
						entries.clear();
						viewer.refresh();
						button.setEnabled(false);
						composite.getDisplay().asyncExec(new Runnable() {
							public void run() {
								try {
									String log = readFile(new File(selected));
									entries.addAll(filterMonitorEntries(log));
								} catch (IOException e1) {
								}
								button.setEnabled(true);
								viewer.refresh();
								viewer.expandAll();
							}
						});
					}
				}
			});
			button.setText("Open log file...");
			button.setEnabled(devMode);
		}
		{
			Label lblThisViewIs = new Label(composite, SWT.NONE);
			lblThisViewIs.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER,
					false, false, 1, 1));
			lblThisViewIs
					.setText("This view is for internal erlide debugging only.");
		}
		{
			viewer = new TreeViewer(composite, SWT.BORDER);
			viewer.setColumnProperties(new String[] { "a", "b" });
			viewer.setContentProvider(new TreeContentProvider());
			Tree tree = viewer.getTree();
			tree.setLinesVisible(true);
			tree.setHeaderVisible(true);
			{
				GridData gridData = new GridData(SWT.FILL, SWT.FILL, false,
						false, 2, 1);
				gridData.heightHint = 223;
				tree.setLayoutData(gridData);
			}
			{
				TreeViewerColumn treeViewerColumn = new TreeViewerColumn(
						viewer, SWT.NONE);
				TreeColumn treeColumn = treeViewerColumn.getColumn();
				treeColumn.setWidth(200);
				treeColumn.setText("Key");
			}
			{
				TreeViewerColumn treeViewerColumn = new TreeViewerColumn(
						viewer, SWT.NONE);
				TreeColumn treeColumn = treeViewerColumn.getColumn();
				treeColumn.setWidth(300);
				treeColumn.setText("Value");
			}
			viewer.setLabelProvider(new ViewerLabelProvider());
			Control ctrl = viewer.getControl();
			{
				GridData gridData = new GridData(SWT.FILL, SWT.FILL, true,
						true, 2, 1);
				gridData.widthHint = 583;
				ctrl.setLayoutData(gridData);
			}
			viewer.setInput(entries);
			viewer.getControl().setEnabled(devMode);
		}
	}

	@Override
	public void setFocus() {
		viewer.getControl().setFocus();
	}

	private File getUsersLatestLog() {
		final String user = System.getProperty("user.name");
		File dir = new File(ErlideUtil.getReportLocation());
		File file = null;
		File[] logs = dir.listFiles(new FilenameFilter() {

			public boolean accept(final File dir, final String name) {
				if (name.startsWith(user)) {
					return true;
				}
				return false;
			}
		});

		long last = 0;
		for (File f : logs) {
			long mod = f.lastModified();
			if (mod > last) {
				last = mod;
				file = f;
			}
		}
		return file;
	}

	private String readFile(final File file) throws IOException {
		BufferedReader reader = new BufferedReader(new FileReader(file));
		try {
			String line = null;
			StringBuilder stringBuilder = new StringBuilder();
			String ls = System.getProperty("line.separator");
			while ((line = reader.readLine()) != null) {
				stringBuilder.append(line);
				stringBuilder.append(ls);
			}
			return stringBuilder.toString();
		} finally {
			reader.close();
		}
	}

	private List<MonitorEntry> filterMonitorEntries(final String log) {
		Pattern hdr = Pattern.compile("\\{erlide_monitor,");
		Pattern tl = Pattern.compile("\\}\\.");
		List<MonitorEntry> result = new ArrayList<MonitorEntry>();
		int end = 0;
		Matcher mh = hdr.matcher(log);
		Matcher mt = tl.matcher(log);
		MonitorEntry prev = null;
		while (true) {
			if (mh.find(end)) {
				int start = mh.start();
				if (mt.find(start)) {
					end = mt.end();
					MonitorEntry entry = new MonitorEntry(prev, log.substring(
							start, end));
					result.add(entry);
					prev = entry;
				} else {
					break;
				}
			} else {
				break;
			}
		}
		return result;
	}

	private static class TreeContentProvider implements ITreeContentProvider {
		private final Object[] EMPTY = new Object[0];

		public void inputChanged(final Viewer viewer, final Object oldInput,
				final Object newInput) {
		}

		public void dispose() {
		}

		public Object[] getElements(final Object inputElement) {
			return getChildren(inputElement);
		}

		@SuppressWarnings("unchecked")
		public Object[] getChildren(final Object parentElement) {
			if (parentElement instanceof List<?>) {
				List<Object> items = (List<Object>) parentElement;
				return items.toArray(new Object[items.size()]);
			}
			if (parentElement instanceof MonitorEntry) {
				return new Object[] {
						new Tuple<String, MonitorEntry>("Processes",
								(MonitorEntry) parentElement),
						new Tuple<String, MonitorEntry>("ETS",
								(MonitorEntry) parentElement),
						new Tuple<String, MonitorEntry>("Memory",
								(MonitorEntry) parentElement),
						new Tuple<String, MonitorEntry>("Statistics",
								(MonitorEntry) parentElement) };
			}
			if (parentElement instanceof Tuple<?, ?>) {
				Tuple<String, MonitorEntry> tpl = (Tuple<String, MonitorEntry>) parentElement;
				if ("Processes".equals(tpl.o1)) {
					return tpl.o2.procs;
				}
				if ("ETS".equals(tpl.o1)) {
					return tpl.o2.ets;
				}
				if ("Memory".equals(tpl.o1)) {
					return tpl.o2.mem;
				}
				if ("Statistics".equals(tpl.o1)) {
					return tpl.o2.stats;
				}
			}
			if (parentElement instanceof OtpErlangList) {
				OtpErlangList list = (OtpErlangList) parentElement;
				return list.elements();
			}
			if (parentElement instanceof OtpErlangTuple) {
				OtpErlangTuple tuple = (OtpErlangTuple) parentElement;
				OtpErlangObject[] elements = tuple.elements();
				if (tuple.elementAt(0) instanceof OtpErlangAtom) {
					Object[] res = new Object[elements.length - 1];
					System.arraycopy(elements, 1, res, 0, elements.length - 1);
					return res;
				}
				return elements;
			}
			if (parentElement instanceof OtpErlangObject) {
				return EMPTY;
			}
			return EMPTY;
		}

		public Object getParent(final Object element) {
			return null;
		}

		public boolean hasChildren(final Object element) {
			return getChildren(element).length > 0;
		}
	}

	private static class ViewerLabelProvider extends LabelProvider {
		@Override
		public Image getImage(final Object element) {
			return super.getImage(element);
		}

		@SuppressWarnings("unchecked")
		@Override
		public String getText(final Object element) {
			if (element instanceof MonitorEntry) {
				return new SimpleDateFormat()
						.format(((MonitorEntry) element).time);
			}
			if (element instanceof Tuple<?, ?>) {
				Tuple<String, MonitorEntry> tpl = (Tuple<String, MonitorEntry>) element;
				return tpl.o1;
			}
			if (element instanceof OtpErlangTuple) {
				OtpErlangTuple tuple = (OtpErlangTuple) element;
				OtpErlangObject[] elements = tuple.elements();
				if (tuple.elementAt(0) instanceof OtpErlangAtom) {
					OtpErlangAtom key = (OtpErlangAtom) tuple.elementAt(0);
					return key.toString();
				}
			}
			return element.toString();
		}
	}

}
