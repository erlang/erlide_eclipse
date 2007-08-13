package org.erlide.ui.views.console;

/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.IColorProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.part.ViewPart;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.IBackendEventListener;
import org.erlide.runtime.backend.console.BackendShell;
import org.erlide.runtime.debug.ErlangProcess;
import org.erlide.ui.prefs.PreferenceConstants;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public class ErlangConsoleView extends ViewPart implements
		IBackendEventListener {

	public static final String ID = "org.erlide.ui.views.console";

	private static final Color[] colors = {
			new Color(Display.getDefault(), 0xFF, 0xFF, 0xFF),
			new Color(Display.getDefault(), 0xCC, 0xFF, 0xFF),
			new Color(Display.getDefault(), 0xFF, 0xCC, 0xFF),
			new Color(Display.getDefault(), 0xFF, 0xFF, 0xCC),
			new Color(Display.getDefault(), 0xCC, 0xCC, 0xFF),
			new Color(Display.getDefault(), 0xCC, 0xFF, 0xCC),
			new Color(Display.getDefault(), 0xFF, 0xCC, 0xCC),
			new Color(Display.getDefault(), 0x99, 0xFF, 0xFF),
			new Color(Display.getDefault(), 0xFF, 0x99, 0xFF),
			new Color(Display.getDefault(), 0xFF, 0xFF, 0x99),
			new Color(Display.getDefault(), 0x99, 0xCC, 0xFF),
			new Color(Display.getDefault(), 0xCC, 0x99, 0xFF),
			new Color(Display.getDefault(), 0xFF, 0x99, 0xCC),
			new Color(Display.getDefault(), 0xFF, 0xCC, 0x99),
			new Color(Display.getDefault(), 0x99, 0xFF, 0xCC),
			new Color(Display.getDefault(), 0xCC, 0xFF, 0x99),
			new Color(Display.getDefault(), 0x99, 0x99, 0xFF),
			new Color(Display.getDefault(), 0xFF, 0x99, 0x99),
			new Color(Display.getDefault(), 0x99, 0xFF, 0x99) };

	private StyledText consoleText;

	private boolean fGroupByLeader;

	private boolean fColored;

	private Set<OtpErlangPid> pids = new TreeSet<OtpErlangPid>();

	private TableViewer consoleTable;

	private ErlConsoleDocument fDoc;

	private IBackend fBackend;

	private BackendShell fShell;

	private List<String> history = new ArrayList<String>(10);

	private StyledText consoleInput;

	public ErlangConsoleView() {
		fDoc = new ErlConsoleDocument();
		fBackend = BackendManager.getDefault().getIdeBackend();
		fBackend.addEventListener("io_server", this);

		try {
			Job j = new Job("shell opener") {
				@Override
				protected IStatus run(IProgressMonitor monitor) {
					fShell = fBackend.getShellManager().openShell("main");
					return Status.OK_STATUS;
				}
			};
			j.setSystem(true);
			j.setPriority(Job.SHORT);
			j.schedule(200);
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public void createPartControl(Composite parent) {
		parent.setLayout(new GridLayout());
		final ToolBar toolbar = new ToolBar(parent, SWT.FLAT);
		toolbar.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));
		toolbar.setLayout(new RowLayout());

		final ToolItem refreshBtn = new ToolItem(toolbar, SWT.PUSH);
		refreshBtn.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				refreshView();
			}

		});
		refreshBtn.setText("Refresh");
		new ToolItem(toolbar, SWT.SEPARATOR);

		final ToolItem colorCheck = new ToolItem(toolbar, SWT.CHECK);
		colorCheck.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				fColored = !fColored;
				refreshView();
			}
		});
		colorCheck.setText("Colored");

		final ToolItem groupChk = new ToolItem(toolbar, SWT.CHECK);
		groupChk.setText("group");
		groupChk.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				fGroupByLeader = !fGroupByLeader;
				groupChk.setText(fGroupByLeader ? "leader" : "sender");
				consoleTable.setInput(fDoc);
				refreshView();
			}
		});
		groupChk.setText(fGroupByLeader ? "leader" : "sender");

		final ToolItem filterItem = new ToolItem(toolbar, SWT.DROP_DOWN);
		filterItem.setText("Filter");

		final Menu filter = new Menu(toolbar);
		addDropDown(filterItem, filter);

		final ToolItem newItemToolItem = new ToolItem(toolbar, SWT.SEPARATOR);
		newItemToolItem.setText("New item");

		final TabFolder tabFolder = new TabFolder(parent, SWT.NONE);
		final GridData gridData = new GridData(SWT.FILL, SWT.FILL, false, true);
		gridData.widthHint = 475;
		gridData.heightHint = 290;
		tabFolder.setLayoutData(gridData);

		final TabItem plainTab = new TabItem(tabFolder, SWT.NONE);
		plainTab.setText("Plain");

		final Composite composite = new Composite(tabFolder, SWT.NONE);
		final GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 2;
		composite.setLayout(gridLayout);
		plainTab.setControl(composite);

		consoleText = new StyledText(composite, SWT.V_SCROLL | SWT.MULTI
				| SWT.READ_ONLY | SWT.BORDER);
		consoleInput = new StyledText(composite, SWT.BORDER | SWT.MULTI
				| SWT.V_SCROLL);

		consoleText.setFont(JFaceResources.getTextFont());
		consoleText.setEditable(false);
		GridData gd1 = new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1);
		int lh1 = consoleText.getLineHeight();
		gd1.minimumHeight = lh1 * 6;
		consoleText.setLayoutData(gd1);
		consoleText.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseDown(MouseEvent e) {
				int ofs = consoleText.getOffsetAtLocation(new Point(e.x, e.y));
				IoRequest req = fDoc.findAtPos(ofs);
				clearMarks();
				if (req.getSender() != null) {
					List<IoRequest> reqs = fDoc.getAllFrom(req.getSender());
					markRequests(reqs);
				}
			}
		});
		consoleText.addMouseTrackListener(new MouseTrackAdapter() {
			@Override
			public void mouseHover(MouseEvent e) {
				try {
					int ofs = consoleText.getOffsetAtLocation(new Point(e.x,
							e.y));
					IoRequest req = fDoc.findAtPos(ofs);
					consoleText.setToolTipText("sent by "
							+ ErlangProcess.toLocalPid(req.getSender()));
				} catch (Exception ex) {
				}
			}
		});
		consoleText.addFocusListener(new FocusAdapter() {
			@Override
			public void focusGained(FocusEvent e) {
				consoleInput.setFocus();
			}
		});

		consoleInput.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.keyCode == 13 && (e.stateMask & SWT.CTRL) == SWT.CTRL) {
					ErlangConsoleView.this.sendInput();
					e.doit = false;
				} else if ((e.keyCode == SWT.ARROW_UP)
						&& ((e.stateMask & SWT.CTRL) == SWT.CTRL)) {
					ConsoleHistoryInformationControl info;
					info = new ConsoleHistoryInformationControl(new Shell(),
							SWT.ON_TOP | SWT.TOOL | SWT.RESIZE, SWT.MULTI
									| SWT.WRAP,
							PreferenceConstants.EDITOR_TEXT_FONT, null,
							ErlangConsoleView.this);
					info.setForegroundColor(Display.getDefault()
							.getSystemColor(SWT.COLOR_INFO_FOREGROUND));
					info.setBackgroundColor(Display.getDefault()
							.getSystemColor(SWT.COLOR_INFO_BACKGROUND));
					info.setInput(ErlangConsoleView.this.getHistory());

					final Point pt = consoleText.toDisplay(consoleText
							.getLocation());
					final Point s = consoleText.getSize();
					info.setLocation(new Point(pt.x, pt.y));
					info.setSize(s.x, s.y);
					info.setVisible(true);
					info.setFocus();
				}
			}
		});
		consoleInput.setFont(JFaceResources.getTextFont());
		consoleInput.setWordWrap(true);
		GridData gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		int lh = consoleInput.getLineHeight();
		gd.heightHint = lh * 4;
		consoleInput.setLayoutData(gd);

		final Composite composite_1 = new Composite(composite, SWT.NONE);
		composite_1.setLayout(new GridLayout());

		final Button sendBtn = new Button(composite_1, SWT.NONE);
		sendBtn.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				ErlangConsoleView.this.sendInput();
			}
		});
		sendBtn.setLayoutData(new GridData(65, 34));
		sendBtn.setToolTipText("Ctrl+Enter");
		sendBtn.setText("Send!");

		final Label sendLabel = new Label(composite_1, SWT.NONE);
		sendLabel.setAlignment(SWT.CENTER);
		sendLabel.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false,
				false));
		sendLabel.setText("Ctrl+Enter");

		final TabItem tracerTab = new TabItem(tabFolder, SWT.NONE);
		tracerTab.setText("Tracer");

		consoleTable = new TableViewer(tabFolder, SWT.BORDER);
		final Table table = consoleTable.getTable();
		tracerTab.setControl(table);
		table.setHeaderVisible(true);
		consoleTable.setContentProvider(new IoRequestContentProvider());
		consoleTable.setLabelProvider(new IoRequestLabelProvider());
		consoleTable.setInput(fDoc);
		Table tbl = (Table) consoleTable.getControl();
		tbl.setFont(JFaceResources.getTextFont());
		tbl.setLinesVisible(true);

	}

	protected void sendInput() {
		String s = consoleInput.getText() + "\n";
		input(s);
		consoleInput.setText("");
		consoleInput.setSelection(0);
		refreshView();
	}

	private void updateTableView() {
		consoleTable.setInput(fDoc);
	}

	private void updateConsoleView() {
		consoleText.setRedraw(false);
		consoleText.setText("");
		for (IoRequest req : fDoc.getContent()) {
			consoleText.append(req.getMessage());
			if (fColored) {
				markRequest(req);
			}
		}
		consoleText.setRedraw(true);
		consoleText.setSelection(consoleText.getCharCount() - 1);
	}

	private Color getColor(OtpErlangPid sender) {
		int ix = 0;
		for (Object element : pids) {
			OtpErlangPid pid = (OtpErlangPid) element;
			if (pid.equals(sender)) {
				break;
			}
			ix++;
		}
		if (ix < colors.length - 1) {
			return colors[ix % 19 + 1];
		} else {
			return colors[0];
		}
	}

	private static void addDropDown(final ToolItem item, final Menu menu) {
		item.addListener(SWT.Selection, new Listener() {
			public void handleEvent(Event event) {
				if (event.detail == SWT.ARROW) {
					Rectangle rect = item.getBounds();
					Point pt = new Point(rect.x, rect.y + rect.height);
					pt = item.getParent().toDisplay(pt);
					menu.setLocation(pt.x, pt.y);
					menu.setVisible(true);
				}
			}
		});
	}

	public void input(String data) {
		data = data.trim();
		fDoc.input(data + "\n");
		addToHistory(data);

		final char[] strb = data.toCharArray();
		final byte[] buff = new byte[data.length() + 1];
		for (int i = 0; i < data.length(); i++) {
			buff[i] = (byte) strb[i];
		}
		buff[data.length()] = '\n';
		try {
			fShell.getOutputStream().write(buff);
		} catch (final IOException e) {
			e.printStackTrace();
		}
	}

	private void refreshView() {
		if (consoleText.isDisposed()) {
			return;
		}
		try {
			updateConsoleView();
			updateTableView();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public void addToHistory(String in) {
		if (history.indexOf(in) != -1) {
			history.remove(in);
		}
		history.add(in);
		if (history.size() > 50) {
			history.remove(0);
		}
	}

	public List<String> getHistory() {
		return history;
	}

	private final class IoRequestContentProvider implements
			IStructuredContentProvider {
		public void dispose() {
		}

		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			fDoc = (ErlConsoleDocument) newInput;

			if (fDoc == null) {
				return;
			}

			Table tbl = (Table) viewer.getControl();
			tbl.setRedraw(false);
			// TODO hmmm the flicker is still there....
			try {
				TableColumn[] cs = tbl.getColumns();
				for (TableColumn cc : cs) {
					cc.dispose();
				}

				for (IoRequest req : fDoc.getContent()) {
					OtpErlangPid pid = fGroupByLeader ? req.getLeader() : req
							.getSender();
					pids.add(pid);
				}

				for (OtpErlangPid pid : pids) {
					TableColumn c;
					c = new TableColumn(tbl, SWT.NONE);
					c.setText(ErlangProcess.toLocalPid(pid));
					c.setData(pid);
					c.setWidth(100);
				}
			} finally {
				tbl.setRedraw(true);
			}

		}

		public Object[] getElements(Object inputElement) {
			return fDoc.getContent().toArray();
		}
	}

	private final class IoRequestLabelProvider implements ITableLabelProvider,
			IColorProvider {
		public void addListener(ILabelProviderListener listener) {
		}

		public void dispose() {
		}

		public boolean isLabelProperty(Object element, String property) {
			return true;
		}

		public void removeListener(ILabelProviderListener listener) {
		}

		public Image getColumnImage(Object element, int columnIndex) {
			return null;
		}

		public String getColumnText(Object element, int columnIndex) {
			if (element instanceof IoRequest) {
				IoRequest req = (IoRequest) element;
				Table tbl = (Table) consoleTable.getControl();
				TableColumn c = tbl.getColumn(columnIndex);
				OtpErlangPid pid = fGroupByLeader ? req.getLeader() : req
						.getSender();
				if (c.getData().equals(pid)) {
					return req.getMessage();
				} else {
					return null;
				}
			}
			return null;
		}

		public Color getBackground(Object element) {
			IoRequest req = (IoRequest) element;
			if (fColored) {
				return getColor(fGroupByLeader ? req.getLeader() : req
						.getSender());
			} else {
				return null;
			}
		}

		public Color getForeground(Object element) {
			// IoRequest req = (IoRequest) element;
			return null;
		}

	}

	public void markRequests(List<IoRequest> reqs) {
		for (Object element0 : reqs) {
			IoRequest element = (IoRequest) element0;
			markRequest(element);
		}
	}

	public void markRequest(IoRequest req) {
		StyleRange range = new StyleRange();
		range.start = req.getStart();
		range.length = req.getLength();
		range.background = getColor(fGroupByLeader ? req.getLeader() : req
				.getSender());
		consoleText.setStyleRange(range);
	}

	public void clearMarks() {
		StyleRange range = new StyleRange();
		range.start = 0;
		range.length = consoleText.getCharCount();
		consoleText.setStyleRange(range);
	}

	public void setInput(String str) {
		consoleInput.setText(str);
		consoleInput.setSelection(str.length());
	}

	@Override
	public void setFocus() {
	}

	public void eventReceived(final OtpErlangObject event) {
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				if (!consoleText.isDisposed()) {
					fDoc.add(event, consoleText.getCharCount());
					refreshView();
				}
			}
		});
	}
}
