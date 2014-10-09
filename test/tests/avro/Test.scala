import com.googlecode.avro.marker._

sealed trait RequestAction extends AvroUnion

case class Get(var key: String)
  extends RequestAction 
  with    AvroRecord

case class Put(var key: String, var value: Array[Byte])
  extends RequestAction
  with    AvroRecord

case class Request(var from: String, var actions: List[RequestAction])
  extends AvroRecord
