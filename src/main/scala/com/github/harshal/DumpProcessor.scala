package com.github.harshal

import xml.pull.{EvElemEnd, EvElemStart, XMLEventReader}

/**
 * @author harshal
 * @date: 2/22/13
 */
class DumpProcessor {
  def extractFromFile(xmlFile:String){
    println("Started extraction...")
    var count = 0
    val reader = new XMLEventReader(io.Source.fromFile(xmlFile))
    val runtime = Runtime.getRuntime
    try{
      while(reader.hasNext){
        try{
          reader.next() match{
            case EvElemStart(_, "page", _, _) => {
              processPage(reader)
              count += 1
              if ( count % 1000000 == 0 )
              {
                printf("%d (%dMb mem, %dMb free)\n", count,
                  (runtime.totalMemory/1024/1024).toInt,
                  (runtime.freeMemory/1024/1024).toInt )
                writeToFile(pageList)
                pageList.clear
              }
            }
            case _ =>
          }
        }catch{
          case e:Exception => {
            println("Exception while reading : "+e.getMessage)
            println(e.getStackTraceString)
          }
        }
      }
    }finally {
      printf("%d (%dMb mem, %dMb free)\n", count,
        (runtime.totalMemory/1024/1024).toInt,
        (runtime.freeMemory/1024/1024).toInt )
      writeToFile(pageList)
    }
  }

  private def processPage(parser : XMLEventReader) {
    var count = 0
    var newId = true
    var title = ""
    var id = ""
    var done = false
    while(parser.hasNext & !done){
      parser.next() match{
        case EvElemStart(_, "title", _, _) => title = getText(parser,"title")
        case EvElemStart(_, "id", _, _) => if (newId) {
          id = getText(parser,"id")
          newId = false
        }
        case EvElemStart(_, "text", _, _) => {
          val text = getText(parser,"text")
          if(!text.startsWith("#REDIRECT")){ //Remove ! to get only redirects
          val anchorList = parseAnchorTextAndAddToMap(text)
            if (!title.equals("")) {
              pageList+=Page(title,id,anchorList)
              title = ""
              id = ""
            }
            else count+=1
          }
        }
        case EvElemEnd(_, "page") => {
          done = true
        }
        case _ =>
      }
    }
    if(count!=0) println(count+" titles absent...")
  }

}
