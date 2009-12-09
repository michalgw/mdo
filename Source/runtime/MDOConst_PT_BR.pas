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

// Vers�o Brasileira
// SILVIO DELGADO - silviodelphi@gmail.com - www.clubedosoftware.com.br


{ @abstract(Translate to Portuguese)
  @author(CARLOS MA��O [cmacao@suprasoft.pt])
  @created(30 Aug 2004) }

unit MDOConst;

interface

uses MDOUtils;

resourcestring
{ generic strings used in code }
  SMDODatabaseEditor = 'Editor de &Base de Dados...';
  SMDOTransactionEditor = 'Editor de &Transa��o...';
 SDatabaseFilter = 'Arquivos BD Firebird (*.fdb;*.gdb;*.fb)|*.fdb;*.gdb;*.fb|Todos os Arquivos (*.*)|*.*';
  SDisconnectDatabase = 'A Base de Dados encontra-se desconectada. Deseja conect�-la e continuar?';
  SCommitTransaction = 'A Transa��o encontra-se ativa. Pretende cancelar a Transa��o e continuar?';
  SExecute = 'E&xecutar';
  SNoDataSet = 'N�o existe associa��o ao Dataset';
  SSQLGenSelect = '� necess�rio selecionar pelo menos um campo chave e um campo a modificar';
  SSQLNotGenerated = 'N�o foram geradas quaisquer senten�as de modifica��o SQL, deseja sair mesmo assim?';
  SMDOUpdateSQLEditor = 'Editor &UpdateSQL...';
  SMDODataSetEditor = 'Editor do &Dataset...';
  SSQLDataSetOpen = 'Imposs�vel determinar nomes de campos para %s';
  SDefaultTransaction = '%s, Por Omiss�o';

{ strings used in error messages}
  SUnknownError = 'Erro desconhecido';
  SFirebirdMissing = 'A biblioteca cliente do Firebird n�o foi encontrada. Por favor, instale o Firebird para poder usar esta funcionalidade';
  SFirebirdInstallMissing = 'A biblioteca de Instala��o do Firebird ibinstall.dll n�o foi encontrada. Por favor, instale o Firebird para poder usar esta funcionalidade';
  SIB60feature = '%s � uma fun��o do InterBase 6. Por favor, fa�a a atualiza��o para o Firebird para poder usar esta funcionalidade';
  SNotSupported = 'Funcionalidade n�o suportada';
  SNotPermitted = 'N�o permitido';
  SFileAccessError = 'Erro no acesso ao arquivo tempor�rio';
  SConnectionTimeout = 'Foi excedido o tempo m�ximo para conex�o � Base de Dados';
  SCannotSetDatabase = 'N�o � poss�vel atribuir a base de dados';
  SCannotSetTransaction = 'N�o � poss�vel atribuir a transa��o';
  SOperationCancelled = 'Opera��o cancelada a pedido do usu�rio';
  SDPBConstantNotSupported = 'A constante DPB (isc_dpb_%s) n�o � suportada';
  SDPBConstantUnknown = 'A constante DPB (%d) � desconhecida';
  STPBConstantNotSupported = 'A constante TPB (isc_tpb_%s) n�o � suportada';
  STPBConstantUnknown = 'A constante TPB (%d) � desconhecida';
  SDatabaseClosed = 'N�o � poss�vel executar a opera��o - Base de dados n�o est� conectada';
  SDatabaseOpen = 'N�o � poss�vel executar a opera��o -- Base de dados est� conectada';
  SDatabaseNameMissing = 'Falta o nome de Base de Dados';
  SNotInTransaction = 'A Transa��o n�o est� ativa';
  SInTransaction = 'A Transa��o est� ativa';
  STimeoutNegative = 'Valor de Tempo Limite n�o pode ser negativo';
  SNoDatabasesInTransaction = 'N�o existem Bases de Dados atribu�das ao componente Transaction';
  SUpdateWrongDB = 'Erro ao atualizar a Base de Dados';
  SUpdateWrongTR = 'Erro ao atualizar a Transa��o. Esperada uma transa��o �nica';
  SDatabaseNotAssigned = 'Bade de Dados n�o atribu�da';
  STransactionNotAssigned = 'Transa��o n�o atribu�da';
  SXSQLDAIndexOutOfRange = '�ndice do XSQLDA fora dos limites';
  SXSQLDANameDoesNotExist = 'Nome de XSQLDA n�o existe (%s)';
  SEOF = 'Fim do arquivo';
  SBOF = '�nicio do arquivo';
  SInvalidStatementHandle = 'Manipulador de senten�a inv�lido';
  SSQLOpen = 'MDOSQL Aberto';
  SSQLClosed = 'MDOSQL Fechado';
  SDatasetOpen = 'Dataset Aberto';
  SDatasetClosed = 'Dataset Fechado';
  SUnknownSQLDataType = 'Tipo de Dado SQL desconhecido (%d)';
  SInvalidColumnIndex = '�ndice de coluna inv�lido (o �ndice excede os limites permitidos)';
  SInvalidParamColumnIndex = '�ndice de par�metro inv�lido (o �ndice excede os limites permitidos)';
  SInvalidDataConversion = 'Convers�o de dados inv�lida';
  SColumnIsNotNullable = 'A Coluna n�o pode ser nula (%s)';
  SBlobCannotBeRead = 'A cadeia Blob n�o p�de ser lida';
  SBlobCannotBeWritten = 'A cadeia Blob n�o p�de ser escrita';
  SEmptyQuery = 'Senten�a SQL vazia';
  SCannotOpenNonSQLSelect = 'N�o � poss�vel executar o "Open" numa senten�a n�o-select. Use ExecQuery';
  SNoFieldAccess = 'Sem acesso ao campo "%s"';
  SFieldReadOnly = 'O Campo "%s" � somente-leitura';
  SFieldNotFound = 'O Campo "%s" n�o foi encontrado';
  SNotEditing = 'N�o est� em modo de edi��o';
  SCannotInsert = 'N�o foi poss�vel inserir no Dataset. (N�o existe senten�a de inser��o SQL)';
  SCannotPost = 'N�o foi poss�vel gravar no Dataset. (N�o existe senten�a de altera��o/inser��o SQL)';
  SCannotUpdate = 'N�o foi poss�vel alterar. (N�o existe senten�a de altera��o SQL)';
  SCannotDelete = 'N�o foi poss�vel excluir no dataset. (N�o existe senten�a de exclus�o SQL)';
  SCannotRefresh = 'N�o foi poss�vel atualizar a linha. (N�o existe senten�a de atualiza��o SQL)';
  SBufferNotSet = 'Buffer n�o definido';
  SCircularReference = 'N�o � permitida refer�ncia circular';
  SSQLParseError = 'Erro no Parser SQL:' + CRLF + CRLF + '%s';
  SUserAbort = 'Opera��o abortada pelo usu�rio';
  SDataSetUniDirectional = 'O Dataset � uni-direcional';
  SCannotCreateSharedResource = 'N�o foi poss�vel criar o recurso partilhado. (Erro do Windows %d)';
  SWindowsAPIError = 'Erro API do Windows. (Erro do Windows %d [$%.8x])';
  SColumnListsDontMatch = 'Listas das Colunas n�o conferem';
  SColumnTypesDontMatch = 'Tipos de Dados das Colunas n�o conferem. (do �ndice: %d; ao �ndice: %d)';
  SCantEndSharedTransaction = 'N�o � poss�vel terminar uma transa��o partilhada sen�o de modo for�ado e similar � a��o TimeoutAction da transa��o';
  SFieldUnsupportedType = 'Tipo de dado do campo n�o suportado';
  SCircularDataLink = 'Refer�ncia circular no DataLink';
  SEmptySQLStatement = 'Senten�a SQL vazia';
  SIsASelectStatement = 'use Open para um Procedimento de Sele��o';
  SRequiredParamNotSet = 'Valor de par�metro requerido n�o atribu�do';
  SNoStoredProcName = 'Nenhum nome de Procedimento associado';
  SIsAExecuteProcedure = 'use ExecProc para procedimentos; use TQuery para procedimentos de sele��o';
  SUpdateFailed = 'Falha na altera��o';
  SNotCachedUpdates = 'CachedUpdates n�o ativado';
  SNotLiveRequest = 'Requisi��o n�o � modic�vel';
  SNoProvider = 'Sem Provider';
  SNoRecordsAffected = 'Nenhum registo afetado';
  SNoTableName = 'Sem nome de tabela atribu�do';
  SCannotCreatePrimaryIndex = 'N�o foi poss�vel criar um �ndice Prim�rio; estes s�o criados automaticamente';
  SCannotDropSystemIndex = 'N�o foi poss�vel excluir o �ndice de sistema';
  STableNameMismatch = 'O nome da tabela n�o confere';
  SIndexFieldMissing = 'Campo indexado em falta';
  SInvalidCancellation = 'N�o � poss�vel cancelar eventos durante o processamento';
  SInvalidEvent = 'Evento inv�lido';
  SMaximumEvents = 'Numero m�ximo de eventos atingido';
  SNoEventsRegistered = 'Nenhum evento registrado';
  SInvalidQueueing = 'Fila de espera inv�lida';
  SInvalidRegistration = 'Registro inv�lido';
  SInvalidBatchMove = 'Batch Move inv�lido';
  SSQLDialectInvalid = 'Dialeto SQL inv�lido';
  SSPBConstantNotSupported = 'Constante SPB n�o suportada';
  SSPBConstantUnknown = 'Constante SPB desconhecida';
  SServiceActive = 'N�o � poss�vel executar a opera��o -- o servi�o n�o est� ativo';
  SServiceInActive = 'N�o � poss�vel executar a opera��o  -- o servi�o est� ativo';
  SServerNameMissing = 'Falta nome do servidor';
  SQueryParamsError = 'Par�metros da consulta SQL incorretos ou faltantes';
  SStartParamsError = 'Par�metros incorretos ou faltantes';
  SOutputParsingError = 'Valor do Buffer de Sa�da inesperado';
  SUseSpecificProcedures = 'In�cio de servi�o gen�rico n�o aplic�vel: Use procedimentos espec�ficos para alterar os par�metros de configura��o';
  SSQLMonitorAlreadyPresent = 'J� existe uma inst�ncia ativa do Monitor SQL';
  SCantPrintValue = 'N�o � poss�vel imprimir o valor';
  SEOFReached = 'EOF alcan�ado';
  SEOFInComment = 'Detectado EOF no coment�rio';
  SEOFInString = 'Detectado EOF na cadeia de texto';
  SParamNameExpected = 'Nome de par�metro esperado';
  SSuccess = 'Execu��o conclu�da com sucesso';
  SDelphiException = 'Exce��o Delphi %s';
  SNoOptionsSet = 'N�o foram selecionadas op��es de instala��o';
  SNoDestinationDirectory = 'Diret�rio destino n�o est� definido';
  SNosourceDirectory = 'Diret�rio de origem n�o est� definido';
  SNoUninstallFile = 'N�o foi definido o nome do arquivo de desinstala��o';
  SOptionNeedsClient = 'O componente %s requer um Cliente para funcionar adequadamente';
  SOptionNeedsServer = 'O componente %s requer um Servidor para funcionar adequadamente';
  SInvalidOption = 'Foi especificada uma op��o inv�lida';
  SInvalidOnErrorResult = 'Valor inesperado retornado por onError';
  SInvalidOnStatusResult = 'Valor inesperado retornado por onStatus';
  SGeneratorNotDefined = 'O gerador %s n�o est� definido';

  SCSVExportNoDataset = 'N�o � poss�vel exportar os dados sem um DataSet dispon�vel.';
  SCSVExportNoFileName = 'N�o � poss�vel exportar os dados sem um arquivo definido.';

  SMercuryVersion = 'Mercury Database Objects RC3';
  SEditSQL = 'Editar SQL';
  SDPBConstantUnknownEx = 'A constante DPB (%s) � desconhecida';
  STPBConstantUnknownEx = 'A constante TPB (%s) � desconhecida';

implementation

end.
