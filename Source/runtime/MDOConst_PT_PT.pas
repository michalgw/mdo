{************************************************************************}
{                                                                        }
{       Borland Delphi Visual Component Library                          }
{       InterBase Express core components                                }
{                                                                        }
{       Copyright (c) 1998-2000 Inprise Corporation                      }
{                                                                        }
{    InterBase Express is based in part on the product                   }
{    Free IB Components, written by Gregory H. Deatz for                 }
{    Hoagland, Longo, Moran, Dunst & Doukas Company.                     }
{    Free IB Components is used under license.                           }
{                                                                        }
{    The contents of this file are subject to the InterBase              }
{    Public License Version 1.0 (the "License"); you may not             }
{    use this file except in compliance with the License. You            }
{    may obtain a copy of the License at http://www.Inprise.com/IPL.html }
{    Software distributed under the License is distributed on            }
{    an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either              }
{    express or implied. See the License for the specific language       }
{    governing rights and limitations under the License.                 }
{    The Original Code was created by InterBase Software Corporation     }
{       and its successors.                                              }
{    Portions created by Inprise Corporation are Copyright (C) Inprise   }
{       Corporation. All Rights Reserved.                                }
{    Contributor(s): Jeff Overcash                                       }
{                    Mercury Database Objects [info@mdolib.com]          }
{                                                                        }
{************************************************************************}

// Transtalte do Portuguese
// CARLOS MA��O - cmacao@suprasoft.pt

{ @abstract(Translate to Portuguese)
  @author(CARLOS MA��O [cmacao@suprasoft.pt])
  @created(30 Aug 2004) }

unit MDOConst;

interface

uses MDOUtils;

resourcestring
{ generic strings used in code }
  SMDODatabaseEditor = 'Editor de &Base de Dados...';
  SMDOTransactionEditor = 'Editor de &Transac��o...';
  SDatabaseFilter = 'Ficheiros BD Firebird (*.fdb;*.gdb;*.fb)|*.fdb;*.gdb;*.fb|Todos os Ficheiros (*.*)|*.*';
  SDisconnectDatabase = 'A Base de Dados encontra-se desligada. Pretende lig�-la e continuar?';
  SCommitTransaction = 'A Transac��o encontra-se activa. Pretende Anular a Transac��o e continuar?';
  SExecute = 'E&xecutar';
  SNoDataSet = 'N�o existe associa��o ao dataset';
  SSQLGenSelect = 'Tem que seleccionar pelo menos um campo chave e um campo a modificar';
  SSQLNotGenerated = 'N�o foram geradas quaisquer senten�as de modifica��o SQL, pretende sair mesmo assim?';
  SMDOUpdateSQLEditor = 'Editor &UpdateSQL...';
  SMDODataSetEditor = 'Editor do &Dataset...';
  SSQLDataSetOpen = 'Imposs�vel determinar nomes de campos para %s';
  SDefaultTransaction = '%s, Por Omiss�o';

{ strings used in error messages}
  SUnknownError = 'Erro desconhecido';
  SFirebirdMissing = 'A biblioteca cliente do Firebird n�o foi encontrada. Por favor instale o Firebird para poder usar esta funcionalidade';
  SFirebirdInstallMissing = 'A biblioteca de Instala��o do Firebird ibinstall.dll n�o foi encontrada. Por favor instale o Firebird 6 para poder usar esta funcionalidade';
  SIB60feature = '%s � uma fun��o do InterBase 6. Por favor fa�a a actualiza��o para o Firebird 6 para poder usar esta funcionalidade';
  SNotSupported = 'Funcionalidade n�o suportada';
  SNotPermitted = 'N�o permitido';
  SFileAccessError = 'Erro no acesso ao ficheiro Tempor�rio';
  SConnectionTimeout = 'Foi excedido o tempo m�ximo para liga��o a Base de Dados';
  SCannotSetDatabase = 'N�o � poss�vel atribuir a base de dados';
  SCannotSetTransaction = 'N�o � poss�vel atribuir a transac��o';
  SOperationCancelled = 'Opera��o Cancelada a pedido do utilizador';
  SDPBConstantNotSupported = 'A Constante DPB (isc_dpb_%s) n�o � suportada';
  SDPBConstantUnknown = 'A Constante DPB (%d) � desconhecida';
  STPBConstantNotSupported = 'A Constante TPB (isc_tpb_%s) n�o � suportada';
  STPBConstantUnknown = 'A Constante TPB (%d) � desconhecida';
  SDatabaseClosed = 'N�o � poss�vel executar a opera��o -- A BD n�o se encontra ligada';
  SDatabaseOpen = 'N�o � poss�vel executar a opera��o -- A BD encontra-se ligada';
  SDatabaseNameMissing = 'Nome de Base de Dados em falta';
  SNotInTransaction = 'A Transac��o n�o est� activa';
  SInTransaction = 'A Transac��o est� activa';
  STimeoutNegative = 'Valor de Tempo Limite n�o podem ser negativos';
  SNoDatabasesInTransaction = 'N�o existem Bases de Dados atribu�das ao componente transac��o';
  SUpdateWrongDB = 'Modificando a Base de Dados incorrecta';
  SUpdateWrongTR = 'Modificando a transac��o incorrecta. Esperada uma transac��o �nica';
  SDatabaseNotAssigned = 'Bade de Dados n�o atribu�da';
  STransactionNotAssigned = 'Transac��o n�o atribu�da';
  SXSQLDAIndexOutOfRange = '�ndice do XSQLDA fora dos limites';
  SXSQLDANameDoesNotExist = 'Nome de XSQLDA n�o existe (%s)';
  SEOF = 'Fim do Ficheiro';
  SBOF = '�nicio do Ficheiro';
  SInvalidStatementHandle = 'Descritor de senten�a Inv�lido';
  SSQLOpen = 'MDOSQL Aberto';
  SSQLClosed = 'MDOSQL Fechado';
  SDatasetOpen = 'Dataset Aberto';
  SDatasetClosed = 'Dataset Fechado';
  SUnknownSQLDataType = 'Tipo de Dados SQL desconhecido (%d)';
  SInvalidColumnIndex = '�ndice de coluna inv�lido (o �ndice excede os limites permitidos)';
  SInvalidParamColumnIndex = '�ndice de par�metro inv�lido (o �ndice excede os limites permitidos)';
  SInvalidDataConversion = 'Convers�o de dados inv�lida';
  SColumnIsNotNullable = 'A Coluna n�o pode ser nula (%s)';
  SBlobCannotBeRead = 'A cadeia Blob n�o p�de ser lida';
  SBlobCannotBeWritten = 'A cadeia Blob n�o p�de ser escrita';
  SEmptyQuery = 'Senten�a SQL vazia';
  SCannotOpenNonSQLSelect = 'N�o � poss�vel "open" uma senten�a n�o-select. Use ExecQuery';
  SNoFieldAccess = 'Sem acesso ao campo "%s"';
  SFieldReadOnly = 'O Campo "%s" � apenas-de-leitura';
  SFieldNotFound = 'O Campo "%s" n�o foi encontrado';
  SNotEditing = 'N�o est� em modo de edi��o';
  SCannotInsert = 'N�o foi poss�vel inserir no dataset. (N�o existe senten�a de inser��o SQL)';
  SCannotPost = 'N�o foi poss�vel gravar no dataset. (N�o existe senten�a de modifica��o/inser��o SQL)';
  SCannotUpdate = 'N�o foi poss�vel modificar. (N�o existe senten�a de modifica��o SQL)';
  SCannotDelete = 'N�o foi poss�vel suprimir no dataset. (N�o existe senten�a de supress�o SQL)';
  SCannotRefresh = 'N�o foi poss�vel actualizar a linha. (N�o existe senten�a de actualiza��o SQL)';
  SBufferNotSet = 'Buffer n�o definido';
  SCircularReference = 'N�o � permitida refer�ncia circular';
  SSQLParseError = 'Erro no Parser SQL:' + CRLF + CRLF + '%s';
  SUserAbort = 'Opera��o abortada pelo utilizador';
  SDataSetUniDirectional = 'O Dataset � uni-direccional';
  SCannotCreateSharedResource = 'N�o foi poss�vel criar o recurso partilhado. (Erro do Windows %d)';
  SWindowsAPIError = 'Erro API do Windows. (Erro do Windows %d [$%.8x])';
  SColumnListsDontMatch = 'Listas das Colunas n�o conferem';
  SColumnTypesDontMatch = 'Tipos de Dados das Colunas n�o conferem. (Do �ndice: %d; Ao �ndice: %d)';
  SCantEndSharedTransaction = 'N�o � poss�vel terminar uma transac��o partilhada sen�o de modo for�ado e similar � ac��o TimeoutAction da transac��o';
  SFieldUnsupportedType = 'Tipo de Dados do Campo n�o suportado';
  SCircularDataLink = 'Refer�ncia Circular no DataLink';
  SEmptySQLStatement = 'Senten�a SQL Vazia';
  SIsASelectStatement = 'use Open para um Procedimento de Selec��o';
  SRequiredParamNotSet = 'Valor de Par�metro obrigat�rio n�o atribu�do';
  SNoStoredProcName = 'Nenhum Nome de Procedimento associado';
  SIsAExecuteProcedure = 'use ExecProc para Procedimentos; use TQuery para Procedimentos de Selec��o';
  SUpdateFailed = 'Modifica��o fracassada';
  SNotCachedUpdates = 'CachedUpdates n�o activados';
  SNotLiveRequest = 'O Pedido n�o � modic�vel';
  SNoProvider = 'Sem Provider';
  SNoRecordsAffected = 'Nenhum registo Afectado';
  SNoTableName = 'N�o existe Nome de Tabela atribu�do';
  SCannotCreatePrimaryIndex = 'N�o foi poss�vel criar um �ndice Prim�rio; estes s�o criados automaticamente';
  SCannotDropSystemIndex = 'N�o foi poss�vel Suprimir o �ndice de Sistema';
  STableNameMismatch = 'O Nome da Tabela n�o confere';
  SIndexFieldMissing = 'Campo indexado em falta';
  SInvalidCancellation = 'N�o � poss�vel cancelar eventos durante o processamento';
  SInvalidEvent = 'Evento Inv�lido';
  SMaximumEvents = 'Numero M�ximo de Eventos atingido';
  SNoEventsRegistered = 'Nenhum Evento Registado';
  SInvalidQueueing = 'Fila de Espera Inv�lida';
  SInvalidRegistration = 'Registo Inv�lido';
  SInvalidBatchMove = 'Batch Inv�lido';
  SSQLDialectInvalid = 'Dialecto SQL Inv�lido';
  SSPBConstantNotSupported = 'Constante SPB N�o Suportada';
  SSPBConstantUnknown = 'Constante SPB Desconhecida';
  SServiceActive = 'N�o � poss�vel executar a opera��o -- o servi�o n�o est� ligado';
  SServiceInActive = 'N�o � poss�vel executar a opera��o  -- o servi�o est� ligado';
  SServerNameMissing = 'Nome do Servidor em falta';
  SQueryParamsError = 'Par�metros da Consulta SQL incorrectos ou em falta';
  SStartParamsError = 'Par�metros incorrectos ou em falta';
  SOutputParsingError = 'Valor do Buffer de Sa�da inesperado';
  SUseSpecificProcedures = 'Inicio de Servi�o Gen�rico n�o aplic�vel: Use Procedimentos Espec�ficos para modificar os par�metros de configura��o';
  SSQLMonitorAlreadyPresent = 'J� existe uma inst�ncia activa do Monitor SQL';
  SCantPrintValue = 'N�o � poss�vel imprimir o valor';
  SEOFReached = 'SEOFReached';
  SEOFInComment = 'Detectado EOF no coment�rio';
  SEOFInString = 'Detectado EOF na cadeia de texto';
  SParamNameExpected = 'Esperado nome de Par�metro';
  SSuccess = 'Execu��o terminada com Sucesso';
  SDelphiException = 'Excep��oDelphi %s';
  SNoOptionsSet = 'N�o foram seleccionadas op��es de Instala��o';
  SNoDestinationDirectory = 'N�o est� definida a DirectoriaDestino';
  SNosourceDirectory = 'N�o est� definida a DirectoriaOrigem';
  SNoUninstallFile = 'N�o foi definido o Nome do Ficheiro de Desinstala��o';
  SOptionNeedsClient = 'O componente %s requer um Cliente para funcionar convenientemente';
  SOptionNeedsServer = 'O componente %s requer um Servidor para funcionar convenientemente';
  SInvalidOption = 'Foi especificada uma Op��o Inv�lida';
  SInvalidOnErrorResult = 'Valor inesperado retornado por onError';
  SInvalidOnStatusResult = 'Valor inesperado retornado por onStatus';
  SGeneratorNotDefined = 'O Gerador %s n�o est� definido';

  SCSVExportNoDataset = 'N�o � poss�vel exportar os dados sem um DataSet dispon�vel.';
  SCSVExportNoFileName = 'N�o � poss�vel exportar os dados sem um arquivo definido.';
  
  SMercuryVersion = 'Mercury Database Objects RC1';
  SEditSQL = 'Edita SQL';
  SDPBConstantUnknownEx = 'A constante DPB (%s) � desconhecida';
  STPBConstantUnknownEx = 'A constante TPB (%s) � desconhecida';

implementation

end.
