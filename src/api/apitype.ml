type date = string

type entity = { name : string ; id : int }

type walletJournal =
  {
    date : date ;
    refID : int ;
    refTypeID : int ;
    owner1 : entity ;
    owner2 : entity ;
    argument : entity ;
    amount : float ;
    balance : float ;
    reason : string ;
    taxReceiverID : int ;
    taxAmount : float ;
  }
