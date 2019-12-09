using System;

namespace Minecraft
{
    #region dependencies
    public enum MobType
    {
        PLAYER,
        PIG,
        COW,
        OCELOT,
        ZOMBIE,
        CREEPER
    }
    #endregion

    public class Entity
    {
        #region Constructor

        public MobType Type;

        protected MobType type
        {
            get { return type;}
            set { type = value; }
        }
        public int Hp
        {
            get { return hp;}
            set
            {
                hp -= value;
                if (hp <= 0)
                {
                    hp = 0;
                    IsKO = false;
                }
            }
        }
        protected int hp;
        protected string noise;
        public bool IsKO { get; set; }
        protected bool isKO;
        protected Blocks loot;

        public Entity(MobType type,int hp,string noise, Blocks loot)
        {
            // FIXME
        }
        #endregion
        
        #region Methods

        public virtual void WhoAmI()
        {
            System.Console.WriteLine("I am an entity ! " + this.noise);
        }
        
        public virtual void Describe()
        {
            string A_An()
            {
                string s = (string) this.Type;
                if (s[0] == 'a' || s[0] == 'e' || s[0] == 'i' || s[0] == 'o' || s[0] == 'u' || s[0] == 'y' || s[0] == 'h')
                {
                    return "an";
                }
                return "a";
            }
            System.Console.WriteLine("I'm "+A_An()+" " + this.Type + " and I have " + this.Hp + " hp");
        }
        
        public Blocks GetHurt(int count)
        {
            this.Hp -= count;
            if (Hp == 0)
            {
                return this.loot;
            }
            return new Blocks(BlockType.NONE, 0);
        }
        
        #endregion

    }}