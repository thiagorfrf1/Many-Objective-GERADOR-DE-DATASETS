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
               
                         <font><h2>Ferramenta para geração de conjuntos de dados sintéticos baseados em complexidade</h2></font> 
               
                          <font color="grey"><h2>Configuração do conjunto de dados:</h2></font>
                             <table border="0">
                    <thead>
                        <tr>
                            <th colspan="3"><h1></h1></th>
                        </tr>
                        <tr>
                            <th colspan="3"></th>
                        </tr>
                    </thead>
                    <tbody>

                        <tr bgcolor="#E0EEEE">
                            <td><h3>Quantidade de instâncias</h3></td>
                            <td ><input type="text" name=""class="form-control"></td>
                                 <td width="20px"><img src="img/1.png" alt="" width="25px"/></td>
                        </tr>
                        <tr>
                            <td><h3>Quantidade de classes </h3></td>
                            <td><input type="text" name=""class="form-control"></td>
                                 <td width="20px"><img src="img/1.png" alt="" width="25px"/></td>
                        </tr>
                        <tr bgcolor="#E0EEEE">
                            <td><h3>Quantidade de atributos</h3></td>
                            <td><input type="text" name=""class="form-control"></td>
                                 <td width="20px"><img src="img/1.png" alt="" width="25px"/></td>
                        </tr>  
                        <tr>
                            <td><h3>Tipo de atributo</h3></td>
                            <td>
                                <select class="form-control" name="Tipodeatributo">
                                <option>Escolha aqui</option>
                                <option></option>
                             </td>
                                 <td width="20px"><img src="img/1.png" alt="" width="25px"/></td>
                    </select>
                    
                        </tr>
                        <tr>
                            <td></td>
                            <td></td>
                        </tr>
                    </tbody>
                </table>
                         <table>
                             <tr>
                                 <td><a href="metrica.php"><button class="btn btn-primary"> Voltar</button></a></td>
                             <td>
                                 
                             </td>
                                 <td><button class="btn btn-success"> Concluir</button></td>
                             </tr>
                         </table>
            </form>
        </center>
    </body>
</html>
