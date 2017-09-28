graph [
  directed 1
  id 0
  label "com.appliedminds.martini.DrawableGraph@57e787"
  node [
    id 1
    data [
      label "General Lee Byong-In"
    ]
  ]
  node [
    id 2
    data [
      label "Taepo dong missile program"
    ]
  ]
  node [
    id 3
    data [
      label "Djandjgava Guivi Ivlianovich"
    ]
  ]
  node [
    id 4
    data [
      label "Ramenskoye Design Company"
    ]
  ]
  node [
    id 5
    data [
      label "NK radio intercept at missile base"
    ]
  ]
  node [
    id 6
    data [
      label "missile guidance systems"
    ]
  ]
  edge [
    source 1
    target 2
    data [
      label "heads"
    ]
  ]
  edge [
    source 1
    target 3
    data [
      label "has met with"
    ]
  ]
  edge [
    source 3
    target 4
    data [
      label "is the president of"
      types "(document)"
      typeVisibilities  "(true)"
      typeURLs "(http://www.airshow.ru/expo/309/anketa.htm)"      
    ]
  ]
  edge [
    source 5
    target 4
    data [
      label "mentions"
      types "(document, graph)"
      typeVisibilities  "(true, true)"
      typeURLs "(http://www.rockhawk.com/north_korea.htm, file:/var/home/daepark/.hmv/1032994763446.gml)"      
    ]
  ]
  edge [
    source 4
    target 6
    data [
      label "builds"
    ]
  ]
]
