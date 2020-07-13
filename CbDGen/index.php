<!DOCTYPE html>
<!--
To change this license header, choose License Headers in Project Properties.
To change this template file, choose Tools | Templates
and open the template in the editor.
-->
<html>
    <head>
        <meta charset="UTF-8">
    
        <script src="bootstrap-4.1.3/dist/js/bootstrap.min.js" type="text/javascript"></script>
        <script src="bootstrap-4.1.3/dist/js/bootstrap.js" type="text/javascript"></script>
        <script src="bootstrap-4.1.3/dist/js/bootstrap.bundle.min.js" type="text/javascript"></script>
        <script src="bootstrap-4.1.3/dist/js/bootstrap.bundle.js" type="text/javascript"></script>
        <link href="bootstrap-4.1.3/dist/css/bootstrap-grid.css" rel="stylesheet" type="text/css"/>
        <link href="bootstrap-4.1.3/dist/css/bootstrap-reboot.css" rel="stylesheet" type="text/css"/>
        <link href="bootstrap-4.1.3/dist/css/bootstrap-grid.min.css" rel="stylesheet" type="text/css"/>
        <link href="bootstrap-4.1.3/dist/css/bootstrap-reboot.min.css" rel="stylesheet" type="text/css"/>
        <link href="bootstrap-4.1.3/dist/css/bootstrap.css" rel="stylesheet" type="text/css"/>
        <link href="bootstrap-4.1.3/dist/css/bootstrap.min.css" rel="stylesheet" type="text/css"/>
        <title></title>
    </head>
    <body>
        <nav class="navbar navbar-expand-lg navbar-dark bg-dark py-1">
          <a class="navbar-brand py-0" href="#">CbDGen</a>
        </nav>
        <center>
            <form>
                         <h1>CbDGen</h1>
                         
                         <font color="grey"><h2>Escolha a distribuição dos pontos:</h2></font>
                         <br>
                         <table>
                             <tr>
                                <td width="20px"><h4></h4></td>
                                 <td> <h4>Distribuição</h4> </td>
                                 <td> <center><h4>Exemplo</h4></center> </td>
                         <td width="30px"><center><h4>Ruido</h4></center></td>
                                <td width="30px"><h4></h4></td>
                           
                             </tr>
                             <tr bgcolor="#E0EEEE">
                                 <td><input type="radio"></td>
                                 <td width="150px"><h4>1 - Bolhas de pontos </h4></td>
                                 <td width="55px"><center><img src="img/bolhas-center1.png" alt="" width="55px"/></center></td>
                                 <td><input type="text" name=""class="form-control" placeholder="Centros"></td>
                                 <td width="20px"><img src="img/1.png" alt="" width="25px"/></td>
                             </tr>
                             <tr>
                                 <td><input type="radio"></td>
                                 <td width="150px"><h4>2 - Padrão de luas </h4></td>
                                 <td width="75px"><center><img src="img/redemoinho-ruido01.png" alt="" width="75px"/></center></td>
                                 <td><input type="text" name=""class="form-control" placeholder="0,0 até 0,5"></td>
                                 <td width="20px"><img src="img/1.png" alt="" width="25px"/></td>
                             </tr>
                             <tr bgcolor="#E0EEEE">
                             <td><input type="radio"></td>
                             <td width="150px"><h4>3 - Círculos concêntricos </h4></td>
                             <td width="75px"><center><img src="img/circulo01.png" alt="" width="75px"/></center></td>
                                 <td><input type="text" name=""class="form-control" placeholder="0,0 até 0,5"></td>
                                 <td width="20px"><img src="img/1.png" alt="" width="25px"/></td>
                             </tr>
                             <tr>
                                 <td><input type="radio"></td>
                                 <td width="150px"><h4>4 - Sem distribuição </h4></td>
                                 <td width="75px"><center></center></td>
                                 <td><input type="text" name=""class="form-control" placeholder="0,0 até 0,5"></td>
                                 <td width="20px"><img src="img/1.png" alt="" width="25px"/></td>
                             </tr>
                         </table>
                         <br>        
            </form>
            
                          <table>
                             <tr>
                                 <a href="metrica.php">
                                    <div id="botao">
                                       <td><button class="btn btn-primary"> Próximo</button></td>
                                    </div>
                                 </a>
                             </tr>
                             
                         </table>    
        </center>
    </body>
</html>
