package dfki.protege.ontoviz_tab;

import java.util.*;
import java.io.*;
import java.awt.Color;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;
import dfki.util.*;


class ExportDot implements OptionsTableConstants {

  KnowledgeBase kb;

  boolean standardClass = false;
  boolean systemOwnSlots = false;
  boolean instancesOnly = false;
  boolean slideMode = true; // inverse colors
  boolean slotEdgesDashed = false;
  boolean itsDefaultSlotsHidden = false;
  int itsSlotExtensionDepthInst = 3;
  int itsSlotExtensionDepthCls = 1;
  int itsMaxValues = 3;
  int itsMaxSlots = 5;

  OptionsTable itsOptionsTable;
  SlotConfigTable itsSlotConfigTable;

  Color itsClassNodeColor;
  Color itsInstanceNodeColor;
  Color itsISAEdgeColor;
  Color itsIOEdgeColor;
  Color itsInstanceSlotEdgeColor;
  Color itsClassSlotEdgeColor;


  HashSet itsAcceptableFrames;
  HashSet itsFramesWithSlots;
  HashSet itsFramesWithSlotEdges;
  HashSet itsFramesWithSystemFrames;
  // add here ...

  PrintWriter pw;

  String dotCommand;
  String dotFont;
  String[] dotEnvironment;


  ExportDot(KnowledgeBase kb) {
    this.kb = kb;
    dotCommand = ApplicationProperties.getString("dot.command", "dot");
    dotFont = ApplicationProperties.getString("dot.font", null);
    String dotFontPath = ApplicationProperties.getString("dot.fontpath", null);
    if (dotFontPath != null)
      dotEnvironment = new String[] {"DOTFONTPATH=" + dotFontPath};
    else
      dotEnvironment = null;
  }

  void setStandardClass(boolean standardClass) {
    this.standardClass = standardClass;
  }

  void setSystemOwnSlots(boolean systemOwnSlots) {
    this.systemOwnSlots = systemOwnSlots;
  }

  void setInstancesOnly(boolean instancesOnly) {
    this.instancesOnly = instancesOnly;
  }

  void setSlotExtensionDepthInst(Object depth) {
    if (depth instanceof Integer)
      itsSlotExtensionDepthInst = ((Integer)depth).intValue();
    else // inf
      itsSlotExtensionDepthInst = -1;
  }

  void setSlotExtensionDepthCls(Object depth) {
    if (depth instanceof Integer)
      itsSlotExtensionDepthCls = ((Integer)depth).intValue();
    else // inf
      itsSlotExtensionDepthCls = -1;
  }

  void setMaxValues(Object max) {
    if (max instanceof Integer)
      itsMaxValues = ((Integer)max).intValue();
    else // inf
      itsMaxValues = -1;
  }

  void setMaxSlots(Object max) {
    if (max instanceof Integer)
      itsMaxSlots = ((Integer)max).intValue();
    else // inf
      itsMaxSlots = -1;
  }

  void setSlotConfigTable(SlotConfigTable slotConfigTable) {
    itsSlotConfigTable = slotConfigTable;
  }

  void setDefaultSlotsHidden(boolean hidden) {
    itsDefaultSlotsHidden = hidden;
  }

  void setSlideMode(boolean slideMode) {
    this.slideMode = slideMode;
  }

  void setSlotEdgesDashed(boolean dashed) {
    this.slotEdgesDashed = dashed;
  }

  void setClassNodeColor(Color color) { itsClassNodeColor = color; }
  void setInstanceNodeColor(Color color) { itsInstanceNodeColor = color; }
  void setISAEdgeColor(Color color) { itsISAEdgeColor = color; }
  void setIOEdgeColor(Color color) { itsIOEdgeColor = color; }
  void setInstanceSlotEdgeColor(Color color) { itsInstanceSlotEdgeColor = color; }
  void setClassSlotEdgeColor(Color color) { itsClassSlotEdgeColor = color; }

  void setOptionsTable(OptionsTable optionsTable) {
    itsOptionsTable = optionsTable;
  }


  void export(String fileName) {

    if (itsClassNodeColor == null) itsClassNodeColor = Color.black;
    if (itsInstanceNodeColor == null) itsInstanceNodeColor = Color.red;
    if (itsISAEdgeColor == null) itsISAEdgeColor = Color.black;
    if (itsIOEdgeColor == null) itsIOEdgeColor = Color.pink;
    if (itsInstanceSlotEdgeColor == null) itsInstanceSlotEdgeColor = Color.blue;
    if (itsClassSlotEdgeColor == null) itsClassSlotEdgeColor = Color.red;

    try {

      pw = new PrintWriter(new FileWriter(fileName));

      String kbName = kb.getName();

      pw.println("digraph \"" + kbName + "\" {");

      pw.println();


      if (dotFont != null) {
        pw.println("node [fontname=" + dotFont + "]");
        pw.println("edge [fontname=" + dotFont + "]");
        pw.println();
      }

      generateClosures();
      exportFrames();

      pw.println("}");

      pw.flush();
      pw.close();

    } catch (Exception e) {
      // System.out.println(e);
      e.printStackTrace();
    }

  }


  // dot: call the dot program to add position information

  void dot(String iFileName, String fileName) throws Exception {
    dot(iFileName, fileName, "", true);
  }

  void dot(String iFileName, String fileName, String option, boolean wait)
      throws Exception {
    // option: "-Tgif" etc.
    String[] command =
      new String[] {dotCommand, option, iFileName, "-o", fileName};
    Process process = Runtime.getRuntime().exec(command, dotEnvironment);
    if (wait)
      process.waitFor();
  }


  // closures

  void generateClosures() {

    if (itsOptionsTable.isEmpty()) { // default behavior
      Cls thing = kb.getCls(":THING");
      itsOptionsTable.addFrame(thing);
      itsOptionsTable.setTrue(0, SUB);
    }

    itsAcceptableFrames = new HashSet();
    itsFramesWithSlots = new HashSet();
    itsFramesWithSlotEdges = new HashSet();
    itsFramesWithSystemFrames = new HashSet();

    int l = itsOptionsTable.getRowCount();
    for (int row = 0; row < l; row++) {
      Frame frame = itsOptionsTable.getFrame(row);
      boolean isCls = false;
      Cls cls = null;
      if (frame instanceof Cls) {
	cls = (Cls)frame;
	isCls = true;
      }
      boolean withSlots = itsOptionsTable.isSet(row, SLT);
      boolean withSlotEdges = itsOptionsTable.isSet(row, SLE);
      boolean instances = itsOptionsTable.isSet(row, INS);
      boolean system = itsOptionsTable.isSet(row, SYS);
      // add here ...
      HashSet closure = new HashSet();
      if (system || !isSystem(frame))
        closure.add(frame);
      if (isCls && itsOptionsTable.isSet(row, SUB)) {
        Collection subClasses = cls.getSubclasses();
        addAll(closure, subClasses, system);
      }
      if (isCls && itsOptionsTable.isSet(row, SUP)) {
        Collection superClasses = cls.getSuperclasses();
        addAll(closure, superClasses, system);
      } // else: for instances: add type(s) ... ??? !!!
      if (itsOptionsTable.isSet(row, SLX) || itsOptionsTable.isSet(row, ISX)) {
	int depth;
	if (isCls) depth = itsSlotExtensionDepthCls;
	else depth = itsSlotExtensionDepthInst;
        closure = slotClosure(closure, system, 
	  itsOptionsTable.isSet(row, SLX), // forward
	  itsOptionsTable.isSet(row, ISX), // backward
	  depth);
      }
      if (instances) { // && isCls ??? !!!
        HashSet closureInstances = new HashSet();
        for (Iterator cIterator = closure.iterator();
             cIterator.hasNext();) {
	  Frame cframe = (Frame)cIterator.next();
	  if (cframe instanceof Cls) {
            Cls closureClass = (Cls)cframe;
            addAll(closureInstances, closureClass.getDirectInstances(), system);
	  }
        }
        if (instancesOnly)
          closure = closureInstances; // away with the classes ...
        else
          addAll(closure, closureInstances, system);
      } else { // (!instances)
        if (instancesOnly) {
          // hmmm, computing this closure didn't make much sense
          closure.clear();
        }
      }
      // add closure to acceptable frames etc.
      itsAcceptableFrames.addAll(closure);
      if (withSlots)
        itsFramesWithSlots.addAll(closure);
      if (withSlotEdges)
        itsFramesWithSlotEdges.addAll(closure);
      if (system)
        itsFramesWithSystemFrames.addAll(closure);
      // add here ...
    }

  }


  void addAll(HashSet frames, Collection newFrames, boolean system) {
    for (Iterator frameIterator = newFrames.iterator();
         frameIterator.hasNext();) {
      Frame newFrame = (Frame)frameIterator.next();
      if (system || !isSystem(newFrame))
        frames.add(newFrame);
    }
  }


  HashSet slotClosure(Collection frames, boolean system, 
      boolean forward, boolean backward, int depth) {
    HashSet closure = new HashSet();
    slotClosure(closure, frames, system, forward, backward, depth);
    return closure;
  }

  void slotClosure(HashSet closure, Collection frames, boolean system, 
      boolean forward, boolean backward, int depth) {

    // find all frames that are referred via slots;
    // forward: from current frame / backward: to current frame;
    // recurse up to depth (or infinitely if depth < 0);
    // initially, closure is empty and frames are the frames to
    // begin with

    closure.addAll(frames);

    if (depth == 0 || frames.isEmpty()) // done
      return;

    HashSet extension = new HashSet();
    for (Iterator frameIterator = frames.iterator();
         frameIterator.hasNext();) {
      Frame frame = (Frame)frameIterator.next();
      boolean isCls = false;
      Cls cls = null;
      if (frame instanceof Cls) {
	isCls = true;
	cls = (Cls)frame;
      }
      if (forward) {
      Collection ownSlots = frame.getOwnSlots();
      for (Iterator osIterator = ownSlots.iterator();
	   osIterator.hasNext();) {
	Slot ownSlot = (Slot)osIterator.next();
	if (((system && systemOwnSlots) || !isSystem(ownSlot))
            && acceptedBySlotConfigTable(ownSlot)) {
	  Collection values = frame.getOwnSlotValues(ownSlot);
	  for (Iterator vIterator = values.iterator(); vIterator.hasNext();) {
	    Object value = vIterator.next();
	    if (value instanceof Frame) {
	      Frame valueFrame = (Frame)value;
	      if (system || !isSystem(valueFrame))
	        extension.add(valueFrame);
	    }
	  }
	}
      }
      if (isCls) {
        Collection slots = cls.getTemplateSlots();
        for (Iterator slotIterator = slots.iterator();
             slotIterator.hasNext();) {
          Slot slot = (Slot)slotIterator.next();
          if ((system || !isSystem(slot))
	      && (cls.hasDirectTemplateSlot(slot) ||
	          cls.hasDirectlyOverriddenTemplateSlot(slot))
              && acceptedBySlotConfigTable(slot)) {
            ValueType valueType = cls.getTemplateSlotValueType(slot);
            if (valueType.equals(ValueType.INSTANCE)) {
              Collection allowedClasses = cls.getTemplateSlotAllowedClses(slot);
              for (Iterator acIterator = allowedClasses.iterator();
                   acIterator.hasNext();) {
                Cls allowedClass = (Cls)acIterator.next();
                if (system || !isSystem(allowedClass))
                  extension.add(allowedClass);
              }
            } else if (valueType.equals(ValueType.CLS)) {
              Collection allowedParents = cls.getTemplateSlotAllowedParents(slot);
              for (Iterator apIterator = allowedParents.iterator();
                   apIterator.hasNext();) {
                Cls allowedParent = (Cls)apIterator.next();
                if (system || !isSystem(allowedParent))
                  extension.add(allowedParent);
                addAll(extension, allowedParent.getSubclasses(), system);
              }
            }
          }
	}
      }
      } // forward 
      // backward: WARNING: THIS CODE IS PROBABLY NOT COMPLETE/CORRECT!!!
      if (backward) { // examine referencing frames
	Collection references = frame.getReferences();
	for (Iterator refIterator = references.iterator();
	     refIterator.hasNext();) {
	  MemoryReference ref = (MemoryReference)refIterator.next();
	  Frame refFrame = ref.getFrame();
	  Slot refSlot = ref.getSlot();
	  if (refFrame == null || refSlot == null) continue; // ??
	  // boolean isTemplate = ref.isTemplate(); // ?
	  // a) instance via own slot:
	  if ((system || !isSystem(refFrame))
	      && ((system && systemOwnSlots) || !isSystem(refSlot))
              && acceptedBySlotConfigTable(refSlot)) {
            extension.add(refFrame);
	  }
	  // b) class via template slot:
	  if (refFrame instanceof Slot
	      && refSlot.getName().equals(":SLOT-VALUE-TYPE")) {
	    Slot refSlotFrame = (Slot)refFrame;
	    if ((system || !isSystem(refSlotFrame))
                 && acceptedBySlotConfigTable(refSlotFrame)) {
	      // find frames that use refSlotFrame
	      Collection refReferences = refSlotFrame.getReferences();
	      for (Iterator refRefIterator = refReferences.iterator();
		   refRefIterator.hasNext();) {
		MemoryReference refRef = (MemoryReference)refRefIterator.next();
		Frame refRefFrame = refRef.getFrame();
		Slot refRefSlot = refRef.getSlot();
		if (refRefSlot.getName().equals(":DIRECT-TEMPLATE-SLOTS")
		    && (system || !isSystem(refRefFrame)))
	          extension.add(refRefFrame);
	      }
	    }
	  }
	}
      }
    }

    extension.removeAll(closure); // we only want the really new ones!
    closure.addAll(extension);

    // recurse
    slotClosure(closure, extension, system, forward, backward, depth-1);

  }

  
  // export frames

  void exportFrames() {
    for (Iterator frameIterator = itsAcceptableFrames.iterator();
         frameIterator.hasNext();)
      exportFrame((Frame)frameIterator.next());
  }


  void exportFrame(Frame frame) {

    if (!acceptFrame(frame))
      return;

    Cls cls = null;
    boolean isCls = false;
    if (frame instanceof Cls) {
      cls = (Cls)frame;
      isCls = true;
    }

    Color nodeColor;
    if (isCls) nodeColor = itsClassNodeColor;
    else       nodeColor = itsInstanceNodeColor;

    pw.print(frame(frame));

    // find own slots
    Vector ownSlots = new Vector();
    if (itsFramesWithSlots.contains(frame)) {
      Collection slots = frame.getOwnSlots();
      for (Iterator slotIterator = slots.iterator();
           slotIterator.hasNext();) {
        Slot slot = (Slot)slotIterator.next();
        if (acceptOwnSlot(frame, slot) && (frame.getOwnSlotValueCount(slot) > 0))
          ownSlots.add(slot);
      }
    }

    // find template slots
    Vector templateSlots = new Vector();
    if (isCls && itsFramesWithSlots.contains(cls)) {
      Collection slots = cls.getTemplateSlots();
      for (Iterator slotIterator = slots.iterator();
           slotIterator.hasNext();) {
        Slot slot = (Slot)slotIterator.next();
	if ((cls.hasDirectTemplateSlot(slot) ||
             cls.hasDirectlyOverriddenTemplateSlot(slot))
            && acceptTemplateSlot(cls, slot))
          templateSlots.add(slot);
      }
    }

    if (ownSlots.isEmpty() && templateSlots.isEmpty()) {

      // use a simple box for displaying the node

      pw.print(" [shape=box, label=" + label(frame) + ", "
	+ nodeColorString(nodeColor)
        + "]");

    } else {

      // use a "record" for displaying the node

      pw.print(" [shape=record, "
	+ nodeColorString(nodeColor) + ", "
	+ "label=\"{");

      pw.print(rnormalize(getBrowserText(frame)));

      int scount = 0; // slot counter

      // record part for own slots

      for (Iterator slotIterator = ownSlots.iterator();
           slotIterator.hasNext();) {
        Slot slot = (Slot)slotIterator.next();
        Collection values = frame.getOwnSlotValues(slot);
        pw.print("|{");
        String slotName = rnormalize(slot.getName());
        pw.print(slotName + " =|{" );
	int vcount = 0; // value counter
        for (Iterator vIterator = values.iterator();
             vIterator.hasNext();) {
          Object value = vIterator.next();
          if (value instanceof Instance) {
            pw.print(rnormalize(getBrowserText((Instance)value)));
          } else {
            pw.print(rnormalize(shorten(value.toString())));
          }
	  vcount++;
          if (vIterator.hasNext()) {
            pw.print("|");
	    if (vcount == itsMaxValues) {
	      pw.print("...");
	      break;
	    }
	  }
        }
        pw.print("}}");
	scount++;
	if (slotIterator.hasNext() && scount == itsMaxSlots) {
	  pw.print("|{...}");
	  break;
	}
      }

      // record part for template slots

      for (Iterator slotIterator = templateSlots.iterator();
           slotIterator.hasNext();) {
        Slot slot = (Slot)slotIterator.next();
        pw.print("|{");
        String slotName = rnormalize(slot.getName());
        ValueType valueType = cls.getTemplateSlotValueType(slot);
        pw.print(slotName + "|" + valueType);
        if (cls.getTemplateSlotAllowsMultipleValues(slot))
          pw.print("*");
        if (valueType.equals(ValueType.INSTANCE)) {
              pw.print("|{");
              Collection allowedClasses = cls.getTemplateSlotAllowedClses(slot);
              boolean barNeeded = false;
              for (Iterator acIterator = allowedClasses.iterator();
                   acIterator.hasNext();) {
                Cls allowed = (Cls)acIterator.next();
                // allow ALL classes to be shown (ignore system here ??? !!!):
                if (barNeeded) pw.print("|");
                pw.print(rnormalize(allowed.getName()));
                barNeeded = true;
              }
              pw.print("}");
        } else if (valueType.equals(ValueType.SYMBOL)) {
              pw.print("|{");
              Collection allowedValues = cls.getTemplateSlotAllowedValues(slot);
              boolean barNeeded = false;
	      int count = 0;
              for (Iterator avIterator = allowedValues.iterator();
                   avIterator.hasNext();) {
                String allowed = (String)avIterator.next(); // .toString() ??
                if (barNeeded) pw.print("|");
                pw.print(rnormalize(shorten(allowed)));
                barNeeded = true;
		count++;
		if (avIterator.hasNext() && count == itsMaxValues) {
		  pw.print("|...");
		  break;
		}
              }
              pw.print("}");
        } else if (valueType.equals(ValueType.CLS)) {
              pw.print("|{");
              Collection allowedParents =
                cls.getTemplateSlotAllowedParents(slot);
              boolean barNeeded = false;
              for (Iterator apIterator = allowedParents.iterator();
                   apIterator.hasNext();) {
                Cls allowed = (Cls)apIterator.next();
                if (barNeeded) pw.print("|");
                pw.print(rnormalize(allowed.getName()));
                barNeeded = true;
              }
              pw.print("}");
        }
        pw.print("}");
	scount++;
	if (slotIterator.hasNext() && scount == itsMaxSlots) {
	  pw.print("|{...}");
	  break;
	}
      }

      pw.print("}\"]");

    }

    pw.println();

    // io edge to type
    if (frame instanceof Instance) {
      Cls type = ((Instance)frame).getDirectType();
      if (acceptFrame(type)
          && (standardClass || !type.getName().equals(":STANDARD-CLASS"))) {
  	// plus same for standard rdf class(es) ... ??? !!!
        pw.print(frame(type) + "->" + frame(frame));
        pw.print(" [dir=back, label=io, "
          + "color=" + hsbColor(itsIOEdgeColor) + ", "
          + "fontcolor=" + hsbColor(itsIOEdgeColor)
          + "]");
        pw.println();
      }
    }

    // isa edge to superclasses
    if (isCls) {
      Collection superClasses = cls.getDirectSuperclasses();
      for (Iterator superIterator = superClasses.iterator();
           superIterator.hasNext();) {
        Cls superClass = (Cls)superIterator.next();
        if (acceptFrame(superClass)) {
          pw.print(frame(superClass) + "->" + frame(cls));
          pw.print(" [dir=back, label=isa" + ", "
            + "color=" + hsbColor(itsISAEdgeColor) + ", "
            + "fontcolor=" + hsbColor(itsISAEdgeColor)
            + "]");
          pw.println();
        }
      }
    }

    // own slot edges

    if (itsFramesWithSlotEdges.contains(frame)) {
      Collection slots = frame.getOwnSlots();
      for (Iterator slotIterator = slots.iterator();
           slotIterator.hasNext();) {
        Slot slot = (Slot)slotIterator.next();
        String slotName = label(slot);
        if (acceptOwnSlot(cls, slot)) {
          Collection values = frame.getOwnSlotValues(slot);
          for (Iterator vIterator = values.iterator();
               vIterator.hasNext();) {
            Object value = vIterator.next();
            if (value instanceof Instance) {
              Instance otherInstance = (Instance)value;
              if (acceptFrame(otherInstance)) {
		String edgeColor;
		if (value instanceof Cls)
		  edgeColor =
		    hsbColor(getSlotColor(slot, itsClassSlotEdgeColor));
		else
		  edgeColor = 
		    hsbColor(getSlotColor(slot, itsInstanceSlotEdgeColor));
		boolean back = backwardSlotEdge(slot);
		if (back)
		  pw.print(frame(otherInstance) + "->" + frame(frame));
		else
		  pw.print(frame(frame) + "->" + frame(otherInstance));
                pw.print(" [style=" + slotEdgeStyle() + ", ");
		if (back) pw.print("dir=back, ");
		pw.print("label=" + slotName + ", "
                  + "color=" + edgeColor + ", "
                  + "fontcolor=" + edgeColor
                  + "]");
                pw.println();
              }
            }
          }
        }
      }
    }

    // template slot edges

    if (isCls && itsFramesWithSlotEdges.contains(frame)) {
      Collection slots = cls.getTemplateSlots();
      for (Iterator slotIterator = slots.iterator();
           slotIterator.hasNext();) {
        Slot slot = (Slot)slotIterator.next();
	if ((cls.hasDirectTemplateSlot(slot) ||
	     cls.hasDirectlyOverriddenTemplateSlot(slot))
	    && acceptTemplateSlot(cls, slot)) {
          String slotName =
            label(slot, cls.getTemplateSlotAllowsMultipleValues(slot));
          ValueType valueType = cls.getTemplateSlotValueType(slot);
	  boolean back = backwardSlotEdge(slot);
          if (valueType.equals(ValueType.INSTANCE)) {
            Collection allowedClasses = cls.getTemplateSlotAllowedClses(slot);
            for (Iterator acIterator = allowedClasses.iterator();
                 acIterator.hasNext();) {
              Cls allowed = (Cls)acIterator.next();
              if (acceptFrame(allowed)) {
		Color slotColor = getSlotColor(slot, itsInstanceSlotEdgeColor);
		if (back)
		  pw.print(frame(allowed) + "->" + frame(cls));
		else
		  pw.print(frame(cls) + "->" + frame(allowed));
                pw.print(" [style=" + slotEdgeStyle() + ", ");
		if (back) pw.print("dir=back, ");
		pw.print("label=" + slotName + ", "
                  + "color=" + hsbColor(slotColor) + ", "
                  + "fontcolor=" + hsbColor(slotColor)
                  + "]");
                pw.println();
              }
            }
          } else if (valueType.equals(ValueType.CLS)) {
            Collection allowedParents = cls.getTemplateSlotAllowedParents(slot);
            for (Iterator apIterator = allowedParents.iterator();
                 apIterator.hasNext();) {
              Cls allowed = (Cls)apIterator.next();
              if (acceptFrame(allowed)) {
		Color slotColor = getSlotColor(slot, itsClassSlotEdgeColor);
		if (back)
		  pw.print(frame(allowed) + "->" + frame(cls));
		else
		  pw.print(frame(cls) + "->" + frame(allowed));
                pw.print(" [style=" + slotEdgeStyle() + ", ");
		if (back) pw.print("dir=back, ");
		pw.print("label=" + slotName + ", "
                  + "color=" + hsbColor(slotColor) + ", "
                  + "fontcolor=" + hsbColor(slotColor)
                  + "]");
                pw.println();
              }
            }
          }
        }
      }
    }

  }


  // auxil

  boolean acceptFrame(Frame frame) {
    return itsAcceptableFrames.contains(frame);
  }
	   
  boolean acceptOwnSlot(Frame frame, Slot slot) {
    return (!isSystem(slot) || 
            (systemOwnSlots && itsFramesWithSystemFrames.contains(frame)))
           && acceptedBySlotConfigTable(slot);
  }

  boolean acceptTemplateSlot(Frame frame, Slot slot) {
    return (!isSystem(slot) || itsFramesWithSystemFrames.contains(frame))
	   && acceptedBySlotConfigTable(slot);
  }


  boolean acceptedBySlotConfigTable(Slot slot) {
    if (itsSlotConfigTable == null)
      return !itsDefaultSlotsHidden;
    else
      return itsSlotConfigTable.accept(slot, itsDefaultSlotsHidden);
  }

  Color getSlotColor(Slot slot, Color defaultColor) {
    if (itsSlotConfigTable == null)
      return defaultColor;
    else
      return itsSlotConfigTable.getColor(slot, defaultColor);
  }

  boolean backwardSlotEdge(Slot slot) {
    if (itsSlotConfigTable == null)
      return false;
    else
      return 
	itsSlotConfigTable.getDirection(slot) == SlotConfigConstants.DIR_UP;
  }

  boolean isSystem(Frame frame) {
    // frame.isSystem() is not enough!
    String name = frame.getName();
    return frame.isSystem()
      || name.startsWith(":")
      || name.startsWith("rdf:")
      || name.startsWith("rdfs:")
      // these two are really bad !!!
      || name.equals("RDF Helper")
      || name.equals("URI");
  }

  String frame(Frame frame) {
    return "\"" + normalize(frame.getName()) + "\"";
  }

  String label(Frame frame) {
    return "\"" + normalize(getBrowserText(frame)) + "\"";
  }

  String label(Frame frame, boolean star) {
    return "\"" + normalize(getBrowserText(frame)) +
    (star ? "*" : "") + "\"";
  }

  String getBrowserText(Frame frame) {
    return shorten(frame.getBrowserText());
  }

  String shorten(String string) {
    int max = 40;
    if (string.length() > max)
      string = string.substring(0,max) + "...";
    return string;
  }

  String normalize(String string) {
    StringBuffer buffer = new StringBuffer();
    int l = string.length();
    for (int i = 0; i < l; i++) {
      char c = string.charAt(i);
      switch (c) {
        case '\n':
          buffer.append("\\n");
          break;
        case '"':
        case '<':
        case '>':
        case '\\':
          buffer.append("\\" + c);
          break;
        default:
          buffer.append(c);
      }
    }
    return buffer.toString();
  }

  // normalization for records (i.e., escape {, }, |

  String rnormalize(String string) {
    StringBuffer buffer = new StringBuffer();
    int l = string.length();
    for (int i = 0; i < l; i++) {
      char c = string.charAt(i);
      switch (c) {
        case '\n':
          buffer.append("\\n");
          break;
        case '"':
        case '{':
        case '}':
        case '|':
        case '<':
        case '>':
        case '\\':
          buffer.append("\\" + c);
          break;
        default:
          buffer.append(c);
      }
    }
    return buffer.toString();
  }


  String hsbColor(Color color) {
    // returns "h s b" string for dot
    float hsb[] =
      Color.RGBtoHSB(color.getRed(), color.getGreen(), color.getBlue(), null);
    return "\"" + hsb[0] + "," + hsb[1] + "," + hsb[2] + "\"";
  }

  String nodeColorString(Color color) {
    if (slideMode) {
      int rgbSum = color.getRed() + color.getGreen() + color.getBlue();
      Color fontColor;
      if (rgbSum > 400)
	fontColor = Color.black;
      else
	fontColor = Color.white;
      return "style=filled, "
	+ "color=" + hsbColor(color) + ", "
	+ "fontcolor=" + hsbColor(fontColor); // ...
    } else {
      return "color=" + hsbColor(color) + ", "
        + "fontcolor=" + hsbColor(color);
    }
  }

  String slotEdgeStyle() {
    if (slotEdgesDashed)
      return "dashed";
    else
      return "filled";
  }


}


