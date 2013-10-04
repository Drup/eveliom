type entity = { name : string ; id : int }

type walletJournal =
  {
    date : Apitime.t ;
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
