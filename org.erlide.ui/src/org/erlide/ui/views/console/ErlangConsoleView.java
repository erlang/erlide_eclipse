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

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.source.IAnnotationHover;
import org.eclipse.jface.text.source.ICharacterPairMatcher;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.jface.viewers.IColorProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
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
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.editors.text.TextSourceViewerConfiguration;
import org.eclipse.ui.part.ViewPart;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.IBackendEventListener;
import org.erlide.runtime.backend.console.BackendShell;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.debug.ErlangProcess;
import org.erlide.ui.editors.erl.ColorManager;
import org.erlide.ui.editors.erl.DoubleClickStrategy;
import org.erlide.ui.editors.erl.ErlContentAssistProcessor;
import org.erlide.ui.editors.erl.ErlDamagerRepairer;
import org.erlide.ui.editors.erl.ErlHighlightScanner;
import org.erlide.ui.editors.erl.ErlJavaPairMatcher;
import org.erlide.ui.editors.erl.ErlangAnnotationHover;
import org.erlide.ui.editors.util.HTMLTextPresenter;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

import erlang.ErlideBackend;

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

	StyledText consoleText;

	boolean fGroupByLeader;

	boolean fColored;

	final Set<OtpErlangPid> pids = new TreeSet<OtpErlangPid>();

	TableViewer consoleTable;

	ErlConsoleDocument fDoc;

	final IBackend fBackend;

	BackendShell fShell;

	final List<String> history = new ArrayList<String>(10);

	StyledText consoleInput;

	SourceViewer consoleInputViewer;

	public ErlangConsoleView() {
		fDoc = new ErlConsoleDocument();
		fBackend = BackendManager.getDefault().getInternalBackend();
		fBackend.addEventListener("io_server", this);

		try {
			final Job j = new Job("shell opener") {
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
	public void createPartControl(final Composite parent) {
		parent.setLayout(new GridLayout());
		final ToolBar toolbar = new ToolBar(parent, SWT.FLAT);
		toolbar.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));
		toolbar.setLayout(new RowLayout());

		final ToolItem refreshBtn = new ToolItem(toolbar, SWT.PUSH);
		refreshBtn.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				refreshView();
			}

		});
		refreshBtn.setText("Refresh");
		new ToolItem(toolbar, SWT.SEPARATOR);

		final ToolItem colorCheck = new ToolItem(toolbar, SWT.CHECK);
		colorCheck.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				fColored = !fColored;
				refreshView();
			}
		});
		colorCheck.setText("Colored");

		final ToolItem groupChk = new ToolItem(toolbar, SWT.CHECK);
		groupChk.setText("group");
		groupChk.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
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

		final SashForm composite = new SashForm(tabFolder, SWT.VERTICAL);
		final GridLayout gridLayout = new GridLayout();
		composite.setLayout(gridLayout);
		plainTab.setControl(composite);

		consoleText = new StyledText(composite, SWT.V_SCROLL | SWT.MULTI
				| SWT.READ_ONLY | SWT.BORDER);

		consoleText.setFont(JFaceResources.getTextFont());
		consoleText.setEditable(false);
		consoleText.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseDown(final MouseEvent e) {
				try {
					final int ofs = consoleText.getOffsetAtLocation(new Point(
							e.x, e.y));
					final IoRequest req = fDoc.findAtPos(ofs);
					clearMarks();
					if (req.getSender() != null) {
						final List<IoRequest> reqs = fDoc.getAllFrom(req
								.getSender());
						markRequests(reqs);
					}
				} catch (final Exception ex) {
				}
			}
		});
		consoleText.addMouseTrackListener(new MouseTrackAdapter() {
			@Override
			public void mouseHover(final MouseEvent e) {
				try {
					final int ofs = consoleText.getOffsetAtLocation(new Point(
							e.x, e.y));
					final IoRequest req = fDoc.findAtPos(ofs);
					consoleText.setToolTipText("sent by "
							+ ErlangProcess.toLocalPid(req.getSender()));
				} catch (final Exception ex) {
				}
			}
		});
		consoleText.addFocusListener(new FocusAdapter() {
			@Override
			public void focusGained(final FocusEvent e) {
				consoleInput.setFocus();
			}
		});
		// consoleInput = new StyledText(composite, SWT.BORDER | SWT.MULTI
		// | SWT.V_SCROLL);

		consoleInputViewer = new SourceViewer(composite, null, SWT.V_SCROLL
				| SWT.BORDER);
		consoleInputViewer.setDocument(fDoc);
		consoleInput = (StyledText) consoleInputViewer.getControl();
		consoleInputViewer
				.configure(new ErlangConsoleSourceViewerConfiguration());
		consoleInput.addKeyListener(new KeyAdapter() {
			boolean historyMode = false;
			int navIndex;
			int lastPos = 0;

			@Override
			public void keyPressed(final KeyEvent e) {
				if (e.keyCode == 13 && isInputComplete(lastPos)
						&& consoleInput.isFocusControl()) {
					sendInput(lastPos);
					e.doit = false;
				} else if (e.keyCode == SWT.ARROW_UP && historyMode) {
					if (navIndex > 0) {
						navIndex--;
					}
					// else {
					// navIndex = history.size() - 1;
					// }
					if (history.size() > navIndex) {
						consoleInput.setText(history.get(navIndex));
						consoleInput.setSelection(consoleInput.getText()
								.length());
					} else {
						consoleInput.setText("");
					}
				} else if (e.keyCode == SWT.ARROW_DOWN && historyMode) {
					// if (navIndex < history.size() - 1) {
					// navIndex++;
					// } else {
					// navIndex = 0;
					// }
					if (navIndex < history.size()) {
						navIndex++;
					}
					final String s = navIndex < history.size() ? history
							.get(navIndex) : "";
					consoleInput.setText(s);
					consoleInput.setSelection(consoleInput.getText().length());
				} else if (e.keyCode == SWT.CTRL) {
					historyMode = true;
					navIndex = history.size();
				} else if (e.keyCode == SWT.ESC) {
					consoleInput.setText("");
				}
				lastPos = consoleInput.getSelection().x;
				super.keyPressed(e);
			}

			@Override
			public void keyReleased(final KeyEvent e) {
				if (e.keyCode == SWT.CTRL) {
					historyMode = false;
				}
				super.keyReleased(e);
			}

		});
		consoleInput.setFont(JFaceResources.getTextFont());
		consoleInput.setWordWrap(true);
		composite.setWeights(new int[] { 200, 100 });

		final TabItem tracerTab = new TabItem(tabFolder, SWT.NONE);
		tracerTab.setText("Tracer");

		consoleTable = new TableViewer(tabFolder, SWT.BORDER);
		final Table table = consoleTable.getTable();
		tracerTab.setControl(table);
		table.setHeaderVisible(true);
		consoleTable.setContentProvider(new IoRequestContentProvider());
		consoleTable.setLabelProvider(new IoRequestLabelProvider());
		consoleTable.setInput(fDoc);
		final Table tbl = (Table) consoleTable.getControl();
		tbl.setFont(JFaceResources.getTextFont());
		tbl.setLinesVisible(true);
		initializeToolBar();
		// initializeToolBar();

	}

	boolean isInputComplete(final int lastPos) {
		try {
			String str = consoleInput.getText();
			str = str.substring(0, lastPos) + str.substring(lastPos + 1).trim()
					+ "\n";
			final OtpErlangObject o = ErlideBackend.parseString(fBackend, str);
			if (o instanceof OtpErlangList && ((OtpErlangList) o).arity() == 0) {
				return false;
			}
		} catch (final BackendException e) {
			return false;
		}
		return true;
	}

	protected void sendInput(final int lastPos) {
		String s = consoleInput.getText();
		s = s.substring(0, lastPos) + s.substring(lastPos + 1).trim() + "\n";
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
		for (final IoRequest req : fDoc.getContentList()) {
			consoleText.append(req.getMessage());
			if (fColored) {
				markRequest(req);
			}
		}
		consoleText.setRedraw(true);
		consoleText.setSelection(consoleText.getCharCount() - 1);
	}

	Color getColor(final OtpErlangPid sender) {
		int ix = 0;
		for (final Object element : pids) {
			final OtpErlangPid pid = (OtpErlangPid) element;
			if (pid.equals(sender)) {
				break;
			}
			ix++;
		}
		if (ix < colors.length - 1) {
			return colors[ix % 19 + 1];
		}
		return colors[0];
	}

	private static void addDropDown(final ToolItem item, final Menu menu) {
		item.addListener(SWT.Selection, new Listener() {
			public void handleEvent(final Event event) {
				if (event.detail == SWT.ARROW) {
					final Rectangle rect = item.getBounds();
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
		fShell.send(data + "\n");
		addToHistory(data);
	}

	void refreshView() {
		if (consoleText.isDisposed()) {
			return;
		}
		try {
			updateConsoleView();
			updateTableView();
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	public void addToHistory(final String in) {
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

	final class IoRequestContentProvider implements IStructuredContentProvider {
		public void dispose() {
		}

		public void inputChanged(final Viewer viewer, final Object oldInput,
				final Object newInput) {
			fDoc = (ErlConsoleDocument) newInput;

			if (fDoc == null) {
				return;
			}

			final Table tbl = (Table) viewer.getControl();
			tbl.setRedraw(false);
			// TODO hmmm the flicker is still there....
			try {
				final TableColumn[] cs = tbl.getColumns();
				for (final TableColumn cc : cs) {
					cc.dispose();
				}

				for (final IoRequest req : fDoc.getContentList()) {
					final OtpErlangPid pid = fGroupByLeader ? req.getLeader()
							: req.getSender();
					pids.add(pid);
				}

				for (final OtpErlangPid pid : pids) {
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

		public Object[] getElements(final Object inputElement) {
			return fDoc.getContentList().toArray();
		}
	}

	final class IoRequestLabelProvider implements ITableLabelProvider,
			IColorProvider {
		public void addListener(final ILabelProviderListener listener) {
		}

		public void dispose() {
		}

		public boolean isLabelProperty(final Object element,
				final String property) {
			return true;
		}

		public void removeListener(final ILabelProviderListener listener) {
		}

		public Image getColumnImage(final Object element, final int columnIndex) {
			return null;
		}

		public String getColumnText(final Object element, final int columnIndex) {
			if (element instanceof IoRequest) {
				final IoRequest req = (IoRequest) element;
				final Table tbl = (Table) consoleTable.getControl();
				final TableColumn c = tbl.getColumn(columnIndex);
				final OtpErlangPid pid = fGroupByLeader ? req.getLeader() : req
						.getSender();
				if (c.getData().equals(pid)) {
					return req.getMessage();
				}
				return null;
			}
			return null;
		}

		public Color getBackground(final Object element) {
			final IoRequest req = (IoRequest) element;
			if (fColored) {
				return getColor(fGroupByLeader ? req.getLeader() : req
						.getSender());
			}
			return null;
		}

		public Color getForeground(final Object element) {
			// IoRequest req = (IoRequest) element;
			return null;
		}

	}

	public void markRequests(final List<IoRequest> reqs) {
		for (final Object element0 : reqs) {
			final IoRequest element = (IoRequest) element0;
			markRequest(element);
		}
	}

	public void markRequest(final IoRequest req) {
		final StyleRange range = new StyleRange();
		range.start = req.getStart();
		range.length = req.getLength();
		range.background = getColor(fGroupByLeader ? req.getLeader() : req
				.getSender());
		consoleText.setStyleRange(range);
	}

	public void clearMarks() {
		final StyleRange range = new StyleRange();
		range.start = 0;
		range.length = consoleText.getCharCount();
		consoleText.setStyleRange(range);
	}

	public void setInput(final String str) {
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

	private void initializeToolBar() {
		// IToolBarManager toolBarManager = getViewSite().getActionBars()
		// .getToolBarManager();
	}

	final class ErlangConsoleSourceViewerConfiguration extends
			TextSourceViewerConfiguration {

		private DoubleClickStrategy doubleClickStrategy;
		private ErlHighlightScanner fHighlightScanner;
		private ICharacterPairMatcher fBracketMatcher;

		/*
		 * (non-Javadoc)
		 * 
		 * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getContentAssistant(org.eclipse.jface.text.source.ISourceViewer)
		 */
		@Override
		public IContentAssistant getContentAssistant(
				final ISourceViewer sourceViewer) {

			final ContentAssistant asst = new ContentAssistant();

			// TODO vi vill ha in en punkt här, men den får return till
			// styledtext o skickar allt för tidigt...
			asst.setContentAssistProcessor(new ErlContentAssistProcessor(
					sourceViewer, ""), IDocument.DEFAULT_CONTENT_TYPE);

			asst.enableAutoActivation(true);
			asst.setAutoActivationDelay(500);
			asst.enableAutoInsert(true);
			asst.enablePrefixCompletion(false);
			// asst.setDocumentPartitioning(IErlangPartitions.ERLANG_PARTITIONING);

			asst
					.setProposalPopupOrientation(IContentAssistant.PROPOSAL_OVERLAY);
			asst
					.setContextInformationPopupOrientation(IContentAssistant.CONTEXT_INFO_ABOVE);
			asst
					.setInformationControlCreator(getInformationControlCreator(sourceViewer));

			return asst;
		}

		@Override
		public IAnnotationHover getAnnotationHover(
				final ISourceViewer sourceViewer) {
			return new ErlangAnnotationHover();
		}

		/*
		 * @see SourceViewerConfiguration#getInformationControlCreator(ISourceViewer)
		 * @since 2.0
		 */
		@Override
		public IInformationControlCreator getInformationControlCreator(
				final ISourceViewer sourceViewer) {
			return new IInformationControlCreator() {

				public IInformationControl createInformationControl(
						final Shell parent) {
					return new DefaultInformationControl(parent, SWT.NONE,
							new HTMLTextPresenter(true));
				}
			};
		}

		/**
		 * The double click strategy
		 * 
		 * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getDoubleClickStrategy(org.eclipse.jface.text.source.ISourceViewer,
		 *      java.lang.String)
		 */
		@Override
		public ITextDoubleClickStrategy getDoubleClickStrategy(
				final ISourceViewer sourceViewer, final String contentType) {
			if (doubleClickStrategy == null) {
				// doubleClickStrategy = new
				// ErlDoubleClickSelector(getBracketMatcher());
				doubleClickStrategy = new DoubleClickStrategy(
						getBracketMatcher());
			}
			return doubleClickStrategy;
		}

		/**
		 * Creates and returns the fHighlightScanner
		 * 
		 * @return the highlighting fHighlightScanner
		 */
		protected ErlHighlightScanner getHighlightScanner() {
			if (fHighlightScanner == null) {
				fHighlightScanner = new ErlHighlightScanner(new ColorManager());
			}
			return fHighlightScanner;
		}

		public ICharacterPairMatcher getBracketMatcher() {
			if (fBracketMatcher == null) {
				// TODO: get the ErlPairMatcher to work in some way
				// final IErlScanner scanner = ErlModelUtils.getScanner(editor);
				// fBracketMatcher = new ErlPairMatcher(scanner);
				fBracketMatcher = new ErlJavaPairMatcher(new String[] { "(",
						")", "{", "}", "[", "]", "<<", ">>" });
			}
			return fBracketMatcher;
		}

		/**
		 * Creates the reconciler
		 * 
		 * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getPresentationReconciler(org.eclipse.jface.text.source.ISourceViewer)
		 */
		@Override
		public IPresentationReconciler getPresentationReconciler(
				final ISourceViewer sourceViewer) {
			final PresentationReconciler reconciler = new PresentationReconciler();

			final ErlHighlightScanner scan = getHighlightScanner();
			if (scan != null) {
				final DefaultDamagerRepairer dr = new ErlDamagerRepairer(scan);
				reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
				reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);
			}
			return reconciler;
		}

	}

	// private void initializeToolBar() {
	// IToolBarManager toolBarManager = getViewSite().getActionBars()
	// .getToolBarManager();
	// }
}
