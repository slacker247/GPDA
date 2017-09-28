graph [
  comment "Created by GMLOutput on Tue Sep 10 18:41:25 PDT 2002"
  directed 1
  id 0
  label "com.appliedminds.martini.DrawableGraph@a45a24"
  node [
    id 2
    label "North Korea is receiving long range missile design assistance from Russia"
    data [
      label "North Korea is receiving long range missile design assistance from Russia"
      manual "false"
      sliderVisible "true"
      sliderValue "2"
    ]
  ]
  node [
    id 3
    label "NK is receiving long range missile design assistance"
    data [
      label "NK is receiving long range missile design assistance"
      manual "false"
      sliderVisible "true"
      sliderValue "100"
    ]
  ]
  node [
    id 4
    label "China may be providing assistance instead"
    data [
      label "China may be providing assistance instead"
      sliderVisible "true"
      sliderValue "100"
    ]
  ]
  node [
    id 5
    label "NK is using guidance systems from Ramenskoye"
    data [
      label "NK is using guidance systems from Ramenskoye"
      sliderVisible "true"
      sliderValue "100"
      types "(document)"
      typeVisibilities "(true)"
      typeURLs "(http://strony.wp.pl/wp/keshels.aviation.page/mig29/29history.htm)"
    ]
  ]
  node [
    id 6
    label "Russia publicly affirms non-proliferation with Rogue States"
    data [
      label "Russia publicly affirms non-proliferation with Rogue States"
      sliderVisible "true"
      sliderValue "100"
    ]
  ]
  node [
    id 7
    label "NK successfully launched Taepo-Dong 2 missile into low earth orbit"
    data [
      label "NK successfully launched Taepo-Dong 2 missile into low earth orbit"
      sliderVisible "true"
      sliderValue "100"
      types "(document)"
      typeURLs "(http://www.cia.gov/cia/public_affairs/speeches/archives/1998/walpole_speech_120898.html)"
      typeVisibilities "(true)"
    ]
  ]
  node [
    id 8
    label "NK has sought missile design assistance in the past"
    data [
      label "NK has sought missile design assistance in the past"
      sliderVisible "true"
      sliderValue "100"
      types "(document)"
      typeURLs "(http://cns.miis.edu/research/korea/msl.htm)"
      label "NK has sought missile design assistance in the past"
      typeVisibilities "(true)"
    ]
  ]
  node [
    id 9
    label "Russian jets have landed near Musudan-ri missile facility"
    data [
      label "Russian jets have landed near Musudan-ri missile facility"
      sliderVisible "true"
      sliderValue "100"
    ]
  ]
  edge [
    source 3
    target 2
    label "supports"
    data [
      positive "true"
      label "supports"
      sliderVisible "false"
      sliderValue "5"
    ]
  ]
  edge [
    source 4
    target 2
    label "refutes"
    data [
      positive "false"
      label "refutes"
      sliderVisible "false"
      sliderValue "5"
    ]
  ]
  edge [
    source 9
    target 2
    label "supports"
    data [
      positive "true"
      label "supports"
      manual "true"
      sliderVisible "false"
      sliderValue "5"
    ]
  ]
  edge [
    source 5
    target 2
    label "supports"
    data [
      positive "true"
      label "supports"
      sliderVisible "false"
      sliderValue "5"
    ]
  ]
  edge [
    source 6
    target 2
    label "refutes"
    data [
      positive "false"
      label "refutes"
      sliderVisible "false"
      sliderValue "5"
    ]
  ]
  edge [
    source 7
    target 3
    label "supports"
    data [
      positive "true"
      label "supports"
      sliderVisible "false"
      sliderValue "5"
    ]
  ]
  edge [
    source 8
    target 3
    label "supports"
    data [
      positive "true"
      label "supports"
      sliderVisible "false"
      sliderValue "5"
    ]
  ]
]
