package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.Play.current
import play.api.db._
import anorm._

import play.api.libs.functional.syntax._

case class Category(name: String, subs: List[String])

object Application extends Controller {
  /*
  implicit val rds = (
    (__ \ 'name).read[String] and
      (__ \ 'age).read[Long]
    ) tupled

  def sum() = Action {
    val nieces = Seq("Aleka", "Christina", "Emily", "Hannah", "Molly")
    Ok(Json.toJson(nieces))
  }

  def sayHello = Action { request =>
    request.body.asJson.map { json =>
      json.validate[(String, Long)].map{
        case (name, age) => Ok("Hello " + name + ", you're "+age)
      }.recoverTotal{
        e => BadRequest("Detected error:"+ JsError.toFlatJson(e))
      }
    }.getOrElse {
      BadRequest("Expecting Json data")
    }
  }


  def tes = Action { request =>
    request.body.asJson.map { json =>
      implicit val personReader: Reads[(String, String)] = (
      (__ \ "firstName").read[String] and
        (__ \ "lastName").read[String]
      ).tupled
      val peoples = (json \ "employees").as[List[(String, String)]]
      peoples.foreach(println)
      Ok("oke")
    }.getOrElse {
      BadRequest("Expecting Json data")
    }
  }

  def dbtes = Action {
    var outString = "Number is "
    val conn = DB.getConnection()
    try {
      val stmt = conn.createStatement
      val rs = stmt.executeQuery("SELECT 9 as testkey ")
      while (rs.next()) {
        outString += rs.getString("testkey")
      }
    } finally {
      conn.close()
    }
    Ok(outString)
  }

  def insert = Action {
    val id: Option[Long] = DB.withConnection { implicit c =>
      SQL("insert into user (android_id, phone_number) values ({symbol}, {companyName})")
        .on("symbol" -> "123123",
          "companyName" -> "asfasdfasf")
        .executeInsert()
    }
    Ok("oke")
  }
*/

  def newUser = Action { request =>
    request.body.asJson.map { json =>
      implicit val personReader: Reads[(String, String)] = (
        (__ \ "id").read[String] and
          (__ \ "number").read[String]
        ).tupled
        json.validate[(String, String)].map{
        case (id, number) => {
          val a: Option[Long] = DB.withConnection { implicit c =>
            SQL("insert into user (android_id, phone_number,email,number) values ({and_id}, {numb},{email},{number})")
              .on("and_id" -> id,
                "numb" -> number,
                "email" -> "",
                "number" -> "")
              .executeInsert()
          }
          Ok("oke")
        }
      }.recoverTotal{
        e => BadRequest("Detected error:"+ JsError.toFlatJson(e))
      }
    }.getOrElse {
      BadRequest("Expecting Json data")
    }
  }

  def selUser = Action {
    var outString = ""
    val conn = DB.getConnection()
    try {
      val stmt = conn.createStatement
      val rs = stmt.executeQuery("SELECT android_id,phone_number FROM user ")
      while (rs.next()) {
        outString += rs.getString("android_id")
        outString += " - "
        outString += rs.getString("phone_number")
      }
    } finally {
      conn.close()
    }
    Ok(outString)
  }

  def newWeb = Action { request =>
    request.body.asJson.map { json =>
      implicit val personReader: Reads[(String, String,String,String)] = (
        (__ \ "id").read[String] and
          (__ \ "title").read[String] and
          (__ \ "url").read[String] and
          (__ \ "time").read[String]
        ).tupled
      json.validate[(String, String,String,String)].map{
        case (id, title,url,time) => {
          var count:Int=0;
          val conn = DB.getConnection()
          try {
            val stmt = conn.createStatement
            val rs = stmt.executeQuery("SELECT * FROM website WHERE client_phone = "+id+" and time = "+time)

            while (rs.next()) {
              count+=1
            }
          } finally {
            conn.close()
          }

          if (count<1) {
            val a: Option[Long] = DB.withConnection { implicit c =>
              SQL("insert into website (client_phone,title,url,time) values ({and_id}, {title}, {url}, {time})")
                .on("and_id" -> id,
                  "title" -> title,
                  "url" -> url,
                  "time" -> time)
                .executeInsert()
            }
            Ok("oke")
          }
          else{
            Ok("duplicate")
          }

        }
      }.recoverTotal{
        e => BadRequest("Detected error:"+ JsError.toFlatJson(e))
      }
    }.getOrElse {
      BadRequest("Expecting Json data")
    }
  }

  def newSMS = Action { request =>
    request.body.asJson.map { json =>
      implicit val personReader: Reads[(String, String,String,String,String,String)] = (
        (__ \ "id").read[String] and
          (__ \ "number").read[String] and
          (__ \ "name").read[String] and
          (__ \ "date").read[String] and
          (__ \ "tipe").read[String] and
          (__ \ "msg").read[String]
        ).tupled
      json.validate[(String, String,String,String,String,String)].map{
        case (id, number,name,date,tipe,msg) => {
          var count:Int=0;
          val conn = DB.getConnection()
          try {
            val stmt = conn.createStatement
            val rs = stmt.executeQuery("SELECT * FROM sms WHERE client_phone = "+id+" and date = "+date)

            while (rs.next()) {
              count+=1
            }
          } finally {
            conn.close()
          }

          if (count<1) {
            val a: Option[Long] = DB.withConnection { implicit c =>
              SQL("insert into sms (client_phone,phone_number,name,date,type,message) values ({and_id}, {numb}, {name}, {date},{tipe},{msg})")
                .on("and_id" -> id,
                  "numb" -> number,
                  "name" -> name,
                  "date" -> date,
                  "tipe" -> tipe,
                  "msg" -> msg)
                .executeInsert()
            }
            Ok("oke")
          }
          else{
            Ok("duplicate")
          }

        }
      }.recoverTotal{
        e => BadRequest("Detected error:"+ JsError.toFlatJson(e))
      }
    }.getOrElse {
      BadRequest("Expecting Json data")
    }
  }

  def newCall = Action { request =>
    request.body.asJson.map { json =>
      implicit val personReader: Reads[(String, String,String,String,String,String)] = (
        (__ \ "id").read[String] and
          (__ \ "number").read[String] and
          (__ \ "name").read[String] and
          (__ \ "date").read[String] and
          (__ \ "duration").read[String] and
          (__ \ "tipe").read[String]
        ).tupled
      json.validate[(String, String,String,String,String,String)].map{
        case (id, number,name,date,duration,tipe) => {
          var count:Int=0;
          val conn = DB.getConnection()
          try {
            val stmt = conn.createStatement
            val rs = stmt.executeQuery("SELECT * FROM phonecall WHERE client_phone = "+id+" and date = "+date)

            while (rs.next()) {
              count+=1
            }
          } finally {
            conn.close()
          }

          if (count<1) {
            val a: Option[Long] = DB.withConnection { implicit c =>
              SQL("insert into phonecall (client_phone,phone_numb,name,date,duration,tipe) values ({and_id}, {numb}, {name}, {date},{duration},{tipe})")
                .on("and_id" -> id,
                  "numb" -> number,
                  "name" -> name,
                  "date" -> date,
                  "duration" -> duration,
                  "tipe" -> tipe)
                .executeInsert()
            }
            Ok("oke")
          }
          else{
            Ok("duplicate")
          }

        }
      }.recoverTotal{
        e => BadRequest("Detected error:"+ JsError.toFlatJson(e))
      }
    }.getOrElse {
      BadRequest("Expecting Json data")
    }
  }

  def showCallData = Action { request =>
    request.body.asJson.map { json =>
      (json \ "id").asOpt[String].map { id =>
        var msg:String ="Phone Details :\n"
        val conn = DB.getConnection()
        try {
          val stmt = conn.createStatement
          val rs = stmt.executeQuery("SELECT phone_numb,name,date,duration,tipe FROM phonecall WHERE client_phone = "+id +" ORDER BY DATE DESC")

          while (rs.next()) {
            msg+= "\n------------------------------\n"

            msg+="Phone Number: " + rs.getString("phone_numb") + "("+rs.getString("name") +")\nCall Type: " + rs.getString("tipe")
            msg+="\nCall Date: " + rs.getString("date") + " \nCall duration in sec : " + rs.getString("duration")
            //sb.append("\n----------------------------------");

          }
        } finally {
          conn.close()
        }

        Ok(msg);
      }.getOrElse {
        BadRequest("Missing parameter [name]")
      }
    }.getOrElse {
      BadRequest("Expecting Json data")
    }
  }

  def showSMSData = Action { request =>
    request.body.asJson.map { json =>
      (json \ "id").asOpt[String].map { id =>
        var msg:String ="SMS Details :\n"
        val conn = DB.getConnection()
        try {
          val stmt = conn.createStatement
          val rs = stmt.executeQuery("SELECT phone_number,name,date,type,message FROM sms WHERE client_phone = "+id+" ORDER BY DATE DESC")

          while (rs.next()) {
            msg+= "\n------------------------------\n"

            msg+="Phone Number: " + rs.getString("phone_number") + "("+rs.getString("name") +")\nDate: " + rs.getString("date")
            msg+="\nType: " + rs.getString("type") + " \nMessage : " + rs.getString("message")
            //sb.append("\n----------------------------------");

          }
        } finally {
          conn.close()
        }
        Ok(msg);
      }.getOrElse {
        BadRequest("Missing parameter [name]")
      }
    }.getOrElse {
      BadRequest("Expecting Json data")
    }
  }



  def showWebData =  Action { request =>
    request.body.asJson.map { json =>
      (json \ "id").asOpt[String].map { id =>
        var msg:String ="Website Details :\n"
        val conn = DB.getConnection()
        try {
          val stmt = conn.createStatement
          val rs = stmt.executeQuery("SELECT title,url,time FROM website WHERE client_phone = "+id+" ORDER BY TIME DESC")

          while (rs.next()) {
            msg+= "\n------------------------------\n"

            msg+="Title: " + rs.getString("title") + "\nAddress : "+rs.getString("url") +"\nDate: " + rs.getString("time")
            //sb.append("\n----------------------------------");

          }
        } finally {
          conn.close()
        }

        Ok(msg);
      }.getOrElse {
        BadRequest("Missing parameter [name]")
      }
    }.getOrElse {
      BadRequest("Expecting Json data")
    }
  }


  def checkAlert = Action { request =>
    request.body.asJson.map { json =>
      (json \ "id").asOpt[String].map { id =>
        var count: Int = 0
        var msg: String = ""
        var number: Set[String] = Set()
        var resp : String = "PERINGATAN!! Terdapat nomor telepon yang tidak dikenal\n"
        val conn = DB.getConnection()
        try {
          val stmt = conn.createStatement
          val rp = stmt.executeQuery("SELECT phone_numb FROM phonecall WHERE client_phone = " + id + " and name = \"\" ")
          var resdum :String=""
          var countdum : Int = 0
          resdum+="\n\nPada daftar telepon : \n"
          while (rp.next()) {
            msg = rp.getString("phone_numb")
            if (number.contains(msg))  {
            }
            else {
              number += msg
              resdum +=msg+"\n"
              count+=1
              countdum+=1
            }
          }
          if (countdum>0)
            resp+=resdum

          number = Set()
          val rs = stmt.executeQuery("SELECT phone_number FROM sms WHERE client_phone = " + id + " and name = \"\" ")
          resdum="\n\nPada daftar SMS : \n"
          countdum = 0
          while (rs.next()) {
            msg = rs.getString("phone_number")
            if (number.contains(msg)) {
            }
            else {
              number+= msg
              resdum +=msg+"\n"
              count+=1
              countdum+=1
            }
          }
          if (countdum>0)
            resp+=resdum
        } finally {
          conn.close()
        }
        if (count==0) {
          Ok("OK")
        }
        else {
          Ok(resp);
        }
      }.getOrElse {
        BadRequest("Missing parameter [name]")
      }
    }.getOrElse {
      BadRequest("Expecting Json data")
    }
  }


  def getBlockedNumber = Action {request =>
    request.body.asJson.map { json =>
      implicit val personReader: Reads[(String, String)] = (
        (__ \ "id").read[String] and
          (__ \ "number").read[String]
        ).tupled
      json.validate[(String, String)].map{
        case (id, number) => {
          var dum:String="";
          val conn = DB.getConnection()
          try {
            val stmt = conn.createStatement
            val rs = stmt.executeQuery("SELECT number FROM user  WHERE android_id = \""+id+"\" and phone_number = \""+number+"\"")

            while (rs.next()) {
              dum+=rs.getString("number")
            }
          } finally {
            conn.close()
          }

          Ok(dum)
        }
      }.recoverTotal{
        e => BadRequest("Detected error:"+ JsError.toFlatJson(e))
      }
    }.getOrElse {
      BadRequest("Expecting Json data")
    }
  }

  def updateUser = Action { request =>
    request.body.asJson.map { json =>
      implicit val personReader: Reads[(String,String, String)] = (
        (__ \ "id").read[String] and
        (__ \ "email").read[String] and
          (__ \ "number").read[String]
        ).tupled
      json.validate[(String, String,String)].map{
        case (id,email, number) => {
          val conn = DB.getConnection()
          try {
            val stmt = conn.createStatement
            stmt.executeUpdate("UPDATE user SET email = \""+email+"\" , number = "+number+" WHERE phone_number = "+id)

        } finally {
            conn.close()
          }
          Ok("oke")
        }
      }.recoverTotal{
        e => BadRequest("Detected error:"+ JsError.toFlatJson(e))
      }
    }.getOrElse {
      BadRequest("Expecting Json data")
    }
  }
}